----------------------------------------------------------------------
--
-- Interface.elm
-- JSON encoder and decoder for Spokes server wire protocol.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Spokes.Server.Interface exposing ( ServerState , emptyServerState
                                        , makeProxyServer, send
                                        , processServerMessage
                                        )

import Spokes.Server.EncodeDecode exposing ( Message(..), encodeMessage )
import Spokes.Server.Error exposing ( ServerError(..), errnum )

import Spokes.Types exposing ( Board, DisplayList, Move(..), RenderInfo
                             , emptyDisplayList )

import Spokes.Board exposing ( renderInfo, computeDisplayList, initialBoard
                             , getNode, isLegalMove, makeMove
                             )


import Dict exposing ( Dict )
import Task

type Phase
    = JoinPhase
    | PlacementPhase
    | ResolutionPhase

type alias ServerState =
    { board : Board
    , renderInfo : RenderInfo
    , phase : Phase
    , players : Int
    , turn : Int
    , resolver : Int
    , placements : Dict Int Move
    , gameid : String
    }

emptyServerState : ServerState
emptyServerState =
    { board = initialBoard
    , renderInfo = renderInfo 600
    , phase = JoinPhase
    , players = 2
    , turn = 1
    , resolver = 1
    , placements = Dict.empty
    , gameid = "<gameid>"
    }

type ServerInterface msg
    = ServerInterface
      { server : String
      , wrapper : ServerInterface msg -> Message -> msg
      , state : Maybe ServerState
      , sender : ServerInterface msg -> Message -> Cmd msg
      }

makeProxyServer : (ServerInterface msg -> Message -> msg) -> ServerInterface msg
makeProxyServer wrapper =
    ServerInterface { server = ""
                    , wrapper = wrapper
                    , state = Nothing
                    , sender = proxySender
                    }

send : ServerInterface msg -> Message -> Cmd msg
send (ServerInterface interface) message =
    interface.sender (ServerInterface interface) message

proxyCmd : ServerInterface msg -> List Message -> Cmd msg
proxyCmd (ServerInterface interface) messages =
    let tasks = List.map Task.succeed messages
        wrapper = interface.wrapper (ServerInterface interface)
        cmds = List.map (Task.perform wrapper) tasks
    in
        Cmd.batch cmds

proxySender : ServerInterface msg -> Message -> Cmd msg
proxySender (ServerInterface interface) message =
    let state = Maybe.withDefault emptyServerState interface.state
        (s2, msgs) = processServerMessage state message
    in
        proxyCmd (ServerInterface { interface | state = Just s2 }) msgs

errorRsp : Message -> ServerError -> String -> Message
errorRsp message error text =
    ErrorRsp { request = encodeMessage message
             , id = errnum error
             , text = text
             }

phaseToString : Phase -> String
phaseToString phase =
    case phase of
        JoinPhase -> "join"
        PlacementPhase -> "placement"
        ResolutionPhase -> "resolution"

checkGameid : ServerState -> Message -> String -> Phase -> Maybe Message
checkGameid state message gameid phase =
    if gameid == state.gameid then
        if phase == state.phase then
            Nothing
        else
            Just
            <| errorRsp message IllegalRequestErr
                ("Not " ++ (phaseToString phase) ++ " phase.")
    else
        Just <| errorRsp message WrongGameidErr "Wrong gameid"

processServerMessage : ServerState -> Message -> (ServerState, List Message)
processServerMessage state message =
    case message of
        -- Basic game play
        NewReq { players } ->
            if players == 2 || players == 4 then
                let st2 = { emptyServerState | players = players }
                    msg = NewRsp { gameid = st2.gameid }
                in
                    -- The non-proxy server will generate a new gameid
                    (st2, [msg])
            else
                ( state
                , [errorRsp message IllegalPlayerCountErr "Players must be 2 or 4"]
                )
        JoinReq { gameid, name } ->
            case checkGameid state message gameid JoinPhase of
                Just err ->
                    (state, [err])
                Nothing ->
                    joinReq state message gameid name
        PlaceReq { gameid, placement, number } ->
            case checkGameid state message gameid JoinPhase of
                Just err ->
                    (state, [err])
                Nothing ->
                    placeReq state message gameid placement number
        ResolveReq { gameid, resolution } ->
            case checkGameid state message gameid ResolutionPhase of
                Just err ->
                    (state, [err])
                Nothing ->
                    resolveReq state message gameid resolution
        {-
        -- Errors
        UndoReq { gameid, message } ->
            messageValue "req" "undo" [ ("gameid", gameid)
                                      , ("message", encodeMessage message)
                                      ]
        UndoRsp { gameid, message } ->
            messageValue "rsp" "undo" [ ("gameid", gameid)
                                      , ("message", encodeMessage message)
                                      ]
        -- Chat
        ChatReq { gameid, text } ->
            messageValue "req" "chat" [ ("gameid", gameid)
                                      , ("text", text)
                                      ]
        ChatRsp { gameid, text, number } ->
            messageValue "rsp" "chat" [ ("gameid", gameid)
                                      , ("text", text)
                                      , ("number", toString number)
                                      ]
         -}
        _ ->
            ( state
            , [errorRsp message IllegalRequestErr "Illegal Request"]
            )

joinReq : ServerState -> Message -> String -> String -> (ServerState, List Message)
joinReq state message gameid name =
    let player = state.resolver
        joinDone = (player == state.players)
        msg = JoinRsp { gameid = gameid
                      , name = name
                      , number = player
                      }
        st2 = { state
                  | resolver = if joinDone then
                                   1
                               else
                                   player + 1
                  , phase = if joinDone then
                                PlacementPhase
                            else
                                JoinPhase
              }
    in
        ( st2
        , if joinDone then
              [ msg
              , PlacephaseRsp { gameid = gameid
                              , turn = st2.turn
                              , resolver = st2.resolver
                              }
              ]
          else
              [ msg ]
        )

placeReq : ServerState -> Message -> String -> Move -> Int -> (ServerState, List Message)
placeReq state message gameid placement number =
    let placements = Dict.insert number placement state.placements
        placeRsp = PlaceRsp { gameid = gameid, number = number }
        done = (state.players == Dict.size placements)
        placementsList = if done then
                             List.map Tuple.second <| Dict.toList placements
                         else
                             []
    in
        case placement of
            Placement _ _ ->
                if isLegalMove placement state.board then
                    ( { state
                          | placements = if done then
                                             Dict.empty
                                         else
                                             placements
                          , phase = if done then
                                        ResolutionPhase
                                    else
                                        PlacementPhase
                          , board = if done then
                                        List.foldl
                                            makeMove state.board placementsList
                                    else
                                        state.board
                      }
                    , if done then
                          [ placeRsp
                          , PlacedRsp { gameid = gameid
                                      , placements = placementsList
                                      }
                          ]
                      else
                          [ placeRsp ]
                    )
                else
                    ( state
                    , [errorRsp message IllegalRequestErr "Illegal Placement"]
                    )

            _ ->
                ( state
                , [errorRsp message IllegalRequestErr "Non-placement move"]
                )

resolveReq : ServerState -> Message -> String -> Move -> (ServerState, List Message)
resolveReq state message gameid resolution =
    case resolution of
        Resolution _ _ _ ->
            if isLegalMove resolution state.board then
                let board = makeMove resolution state.board
                    displayList = computeDisplayList board state.renderInfo
                    done = (displayList.unresolvedPiles == [])
                    phase = if done then
                                PlacementPhase
                            else
                                ResolutionPhase
                    turn = if done then
                               state.turn + 1
                           else
                               state.turn
                    resolver = if done then
                                   (state.resolver % state.players) + 1
                               else
                                   state.resolver
                    resolveRsp = ResolveRsp { gameid = gameid
                                            , resolution = resolution
                                            }
                in
                    ( { state
                          | board = board
                          , phase = phase
                          , turn = turn
                          , resolver = resolver
                      }
                    , if done then
                          [ resolveRsp
                          , PlacephaseRsp { gameid = gameid
                                          , turn = turn
                                          , resolver = resolver
                                          }
                          ]
                      else
                          [ resolveRsp ]
                    )
            else
                ( state
                , [errorRsp message IllegalRequestErr "Illegal Resolution"]
                )
        _ ->
            ( state
            , [errorRsp message IllegalRequestErr "Non-placement move"]
            )
