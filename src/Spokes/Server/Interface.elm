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

module Spokes.Server.Interface exposing ( emptyServerState
                                        , makeProxyServer, send
                                        , processServerMessage
                                        )

import Spokes.Server.EncodeDecode exposing ( encodeMessage )
import Spokes.Server.Error exposing ( ServerError(..), errnum )
import Spokes.Types exposing ( Board, DisplayList, Move(..), StonePile, RenderInfo
                             , History, Message(..)
                             , ServerPhase(..), ServerState, ServerInterface(..)
                             , butLast
                             )
import Spokes.Board exposing ( renderInfo, computeDisplayList, initialBoard
                             , getNode, isLegalMove, makeMove, undoMove
                             )


import Dict exposing ( Dict )
import Task
import List.Extra as LE
import Debug exposing ( log )

emptyServerState : ServerState
emptyServerState =
    { board = initialBoard
    , renderInfo = renderInfo 600
    , phase = JoinPhase
    , unresolvedPiles = []
    , players = 2
    , turn = 1
    , resolver = 1
    , placements = Dict.empty
    , gameid = "<gameid>"
    , history = []
    }

makeProxyServer : (ServerInterface msg -> Message -> msg) -> ServerInterface msg
makeProxyServer wrapper =
    ServerInterface { server = ""
                    , wrapper = wrapper
                    , state = Nothing
                    , sender = proxySender
                    }

send : ServerInterface msg -> Message -> Cmd msg
send ((ServerInterface interface) as si) message =
    interface.sender si message

proxyCmd : ServerInterface msg -> Message -> Cmd msg
proxyCmd ((ServerInterface interface) as si) message =
    let task = Task.succeed message
        wrapper = interface.wrapper si
    in
      Task.perform wrapper task

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

phaseToString : ServerPhase -> String
phaseToString phase =
    case phase of
        JoinPhase -> "join"
        PlacementPhase -> "placement"
        ResolutionPhase -> "resolution"

checkOnlyGameid : ServerState -> Message -> String -> Maybe Message
checkOnlyGameid state message gameid =
    if gameid == state.gameid then
        Nothing
    else
        Just <| errorRsp message WrongGameidErr "Wrong gameid"

checkGameid : ServerState -> Message -> String -> ServerPhase -> Maybe Message
checkGameid state message gameid phase =
    case checkOnlyGameid state message gameid of
        Nothing ->
            if phase == state.phase then
                Nothing
            else
                Just
                <| errorRsp message IllegalRequestErr
                    ("Not " ++ (phaseToString phase) ++ " phase.")
        err ->
            err

processServerMessage : ServerState -> Message -> (ServerState, Message)
processServerMessage state message =
    case message of
        -- Basic game play
        NewReq { players, name } ->
            if players == 2 || players == 4 then
                let st2 = { emptyServerState
                              | players = players
                              , resolver = 2 --auto-join
                          }
                    msg = NewRsp { gameid = st2.gameid
                                 , players = players
                                 , name = name
                                 }
                in
                    -- The non-proxy server will generate a new gameid
                    (st2, msg)
            else
                ( state
                , errorRsp message IllegalPlayerCountErr "Players must be 2 or 4"
                )
        JoinReq { gameid, name } ->
            case checkGameid state message gameid JoinPhase of
                Just err ->
                    (state, err)
                Nothing ->
                    joinReq state message gameid name
        PlaceReq { gameid, placement, number } ->
            case checkGameid state message gameid PlacementPhase of
                Just err ->
                    (state, err)
                Nothing ->
                    placeReq state message gameid placement number
        ResolveReq { gameid, resolution } ->
            case checkGameid state message gameid ResolutionPhase of
                Just err ->
                    (state, err)
                Nothing ->
                    resolveReq state message gameid resolution
        -- Errors
        UndoReq { gameid, message } ->
            case checkOnlyGameid state message gameid of
                Just err ->
                    (state, err)
                Nothing ->
                    undoReq state message gameid message
        -- Chat
        ChatReq { gameid, text, number } ->
            case checkOnlyGameid state message gameid of
                Just err ->
                    (state, err)
                Nothing ->
                    ( state
                    , ChatRsp { gameid = gameid
                              , text = text
                              , number = number
                              }
                    )
        _ ->
            ( state
            , errorRsp message IllegalRequestErr "Illegal Request"
            )

joinReq : ServerState -> Message -> String -> String -> (ServerState, Message)
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
                  , history = if joinDone then
                                  [ { number = 1
                                    , resolver = 1
                                    , placements = []
                                    , resolutions = []
                                    }
                                  ]
                              else
                                  state.history
              }
    in
        ( st2, msg )

placeReq : ServerState -> Message -> String -> Move -> Int -> (ServerState, Message)
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
                    let plcmnts = if done then
                                      Dict.empty
                                  else
                                      placements
                        board = if done then
                                    List.foldl
                                        makeMove state.board placementsList
                                else
                                    state.board
                        (phase, unresolvedPiles) =
                             if done then
                                 let displayList =
                                         computeDisplayList
                                             board state.renderInfo
                                 in
                                     case displayList.unresolvedPiles of
                                         [] ->
                                             (PlacementPhase, [])
                                         piles ->
                                             (ResolutionPhase, piles)
                                else
                                    (PlacementPhase, [])
                        history = if done then
                                      case state.history of
                                          turn :: tail ->
                                              { turn
                                                  | placements = placementsList
                                              } :: tail
                                          history ->
                                              history
                                  else
                                      state.history
                    in
                        ( { state
                              | placements = plcmnts
                              , board = board
                              , phase = phase
                              , unresolvedPiles = unresolvedPiles
                              , history = history
                          }
                        , if done then
                              PlacedRsp { gameid = gameid
                                        , placements = placementsList
                                        }
                          else
                              placeRsp
                        )
                else
                    ( state
                    , errorRsp message IllegalRequestErr "Illegal Placement"
                    )

            _ ->
                ( state
                , errorRsp message IllegalRequestErr "Non-placement move"
                )

isLegalResolution : Move -> List StonePile -> Bool
isLegalResolution move piles =
    case move of
        Resolution _ from _ ->
            case LE.find (\pile -> pile.nodeName == from) piles of
                Nothing ->
                    False
                Just pile ->
                    case pile.resolutions of
                        Nothing ->
                            False
                        Just moves ->
                            List.member move moves
        _ ->
            False

resolveReq : ServerState -> Message -> String -> Move -> (ServerState, Message)
resolveReq state message gameid resolution =
    case resolution of
        Resolution _ _ _ ->
            if not <| isLegalResolution resolution state.unresolvedPiles then
                ( state
                , errorRsp message IllegalRequestErr "Illegal Resolution"
                )
            else
                let board = makeMove resolution state.board
                    displayList = computeDisplayList board state.renderInfo
                    unresolvedPiles = displayList.unresolvedPiles
                    done = (unresolvedPiles == [])
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
                    his = case state.history of
                              turn :: tail ->
                                  { turn
                                      | resolutions =
                                        List.append
                                        turn.resolutions [resolution]
                                  } :: tail
                              _ ->
                                  state.history
                    history = if done then
                                  { number = turn
                                  , resolver = resolver
                                  , placements = []
                                  , resolutions = []
                                  } :: his
                              else
                                  his
                    resolveRsp = ResolveRsp { gameid = gameid
                                            , resolution = resolution
                                            }
                in
                    ( { state
                          | board = board
                          , phase = phase
                          , unresolvedPiles = unresolvedPiles
                          , turn = turn
                          , resolver = resolver
                          , history = history
                      }
                    , resolveRsp
                    )
        _ ->
            ( state
            , errorRsp message IllegalRequestErr "Non-placement move"
            )

undoReq : ServerState -> Message -> String -> Message -> (ServerState, Message)
undoReq state mess originalGameid undoMessage =
    case state.history of
        turn :: tail ->
            case undoMessage of
                PlacedRsp { gameid, placements } ->
                    if gameid == originalGameid &&
                        turn.resolutions == [] &&
                        turn.placements == placements
                    then
                        let board = List.foldl undoMove state.board placements
                            displayList = computeDisplayList board state.renderInfo
                            unresolvedPiles = displayList.unresolvedPiles
                        in
                            ( { state
                                  | board = board
                                  , unresolvedPiles = unresolvedPiles
                                  , phase = PlacementPhase
                                  , history = { turn | placements = [] } :: tail
                              }
                            , UndoRsp { gameid = gameid
                                      , message = undoMessage
                                      }
                            )
                    else
                        ( state
                        , errorRsp mess IllegalRequestErr
                            "Placements don't match"
                        )
                ResolveRsp { gameid, resolution } ->
                    if turn.resolutions == [] then
                        if turn.placements == [] then
                            case tail of
                                tturn :: _ ->
                                    let (st2, response) =
                                            undoReq
                                            { state
                                                | phase = ResolutionPhase
                                                , turn = tturn.number
                                                , resolver = tturn.resolver
                                                , history = tail
                                            }
                                            mess originalGameid undoMessage
                                    in
                                        case response of
                                            ErrorRsp _ ->
                                                (state, response)
                                            _ ->
                                                (st2, response)
                                _ ->
                                    ( state
                                    , errorRsp
                                        mess IllegalRequestErr "No matching history"
                                    )
                        else
                            ( state
                            , errorRsp mess IllegalRequestErr "Not resolution phase"
                            )
                    else
                        case LE.last turn.resolutions of
                            Nothing ->
                                ( state
                                , errorRsp mess IllegalRequestErr "Can't happen"
                                )
                            Just res ->
                                if res == resolution then
                                    let board = undoMove resolution state.board
                                        displayList =
                                            computeDisplayList board state.renderInfo
                                        unresolvedPiles =
                                            displayList.unresolvedPiles
                                    in
                                    ( { state
                                          | board = board
                                          , unresolvedPiles = unresolvedPiles
                                          , history
                                            = { turn
                                                  | resolutions =
                                                      butLast turn.resolutions
                                              } :: tail
                                      }
                                    , UndoRsp { gameid = gameid
                                              , message = undoMessage
                                              }
                                    )
                                else
                                    ( state
                                    , errorRsp mess IllegalRequestErr
                                        "No matching resolution"
                                    )
                _ ->
                    ( state
                    , errorRsp mess IllegalRequestErr
                        "Can only undo placed and resolve responses."
                    )
        _ ->
            ( state
            , errorRsp mess IllegalRequestErr "No history"
            )
