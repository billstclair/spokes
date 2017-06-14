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
                                        , makeProxyServer, makeServer, send
                                        , getServer
                                        , processServerMessage
                                        )

import Spokes.Server.EncodeDecode exposing ( encodeMessage )
import Spokes.Server.Error exposing ( ServerError(..), errnum )
import Spokes.Types as Types
    exposing ( Board, DisplayList, Move(..), StonePile, RenderInfo
             , History, newTurn, Message(..), ServerPhase(..)
             , GameState, ServerState, ServerInterface(..)
             , GameOverReason(..)
             , butLast, adjoin
             )
import Spokes.Board exposing ( renderInfo, computeDisplayList, initialBoard
                             , getNode, isLegalMove, makeMove, undoMove
                             , canResolve, findFullHomeCircle
                             )


import Dict exposing ( Dict )
import Task
import List.Extra as LE
import Debug exposing ( log )
import WebSocket

emptyServerState : ServerState
emptyServerState =
    { playerInfoDict = Dict.empty
    , playeridDict = Dict.empty
    , gameDict = Dict.empty
    , placeOnly = False
    }

dummyGameid : String
dummyGameid =
    "<gameid>"

emptyGameState : GameState
emptyGameState =
    { board = initialBoard
    , renderInfo = renderInfo 600
    , phase = JoinPhase
    , unresolvedPiles = []
    , players = 2
    , resignedPlayers = []
    , turn = 1
    , resolver = 1
    , placements = Dict.empty
    , gameid = dummyGameid
    , history = []
    }

makeProxyServer : (ServerInterface msg -> Message -> msg) -> ServerInterface msg
makeProxyServer wrapper =
    ServerInterface { server = ""
                    , wrapper = wrapper
                    , state = Nothing
                    , sender = proxySender
                    , placeOnly = False
                    }

makeServer : String -> msg -> ServerInterface msg
makeServer server msg =
    ServerInterface { server = server
                    , wrapper = (\_ _ -> msg)
                    , state = Nothing
                    , sender = sender
                    , placeOnly = False
                    }

getServer : ServerInterface msg -> String
getServer (ServerInterface interface) =
    interface.server

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
        (s2, msgs) = processServerMessage
                     { state | placeOnly = interface.placeOnly }
                     message
    in
        proxyCmd (ServerInterface { interface | state = Just s2 }) msgs

sender : ServerInterface msg -> Message -> Cmd msg
sender (ServerInterface interface) message =
    WebSocket.send interface.server (encodeMessage message)

errorRsp : Message -> ServerError -> String -> Message
errorRsp message error text =
    ErrorRsp { request = encodeMessage message
             , id = errnum error
             , text = text
             }

phaseToString : ServerPhase -> String
phaseToString phase =
    case phase of
        StartPhase -> "start"
        JoinPhase -> "join"
        PlacementPhase -> "placement"
        ResolutionPhase -> "resolution"
        ResignedPhase -> "resigned"
        GameOverPhase _ -> "gameover"

checkOnlyGameid : ServerState -> Message -> String -> Result Message GameState
checkOnlyGameid state message gameid =
    case Dict.get gameid state.gameDict of
        Just gameState ->
            Ok gameState
        Nothing ->
            Err <| errorRsp message UnknownGameidErr "Unknown gameid"

checkGameid : ServerState -> Message -> String -> ServerPhase -> Result Message GameState
checkGameid state message gameid phase =
    case checkOnlyGameid state message gameid of
        Ok gameState as res ->
            if phase == gameState.phase then
                res
            else
                Err
                <| errorRsp message IllegalRequestErr
                    ("Not " ++ (phaseToString phase) ++ " phase")
        err ->
            err

checkOnlyPlayerid : ServerState -> Message -> String -> Result Message (GameState, Int)
checkOnlyPlayerid state message playerid =
    case Dict.get playerid state.playerInfoDict of
        Nothing ->
            Err <| errorRsp message BadPlayeridErr "Unknown player id"
        Just { gameid, number } ->
            case checkOnlyGameid state message gameid of
                Err err ->
                    Err err
                Ok gameState ->
                    Ok (gameState, number)

checkPlayerid : ServerState -> Message -> String -> ServerPhase -> Result Message (GameState, Int)
checkPlayerid state message playerid phase =
    case checkOnlyPlayerid state message playerid of
        Err _ as res ->
            res
        Ok (gameState, _) as res ->
            if phase == gameState.phase then
                res
            else
                Err <| errorRsp message IllegalRequestErr
                    ("Not " ++ (phaseToString phase) ++ " phase")

processServerMessage : ServerState -> Message -> (ServerState, Message)
processServerMessage state message =
    case message of
        -- Basic game play
        NewReq { players, name } ->
            if players == 2 || players == 4 then
                let info = emptyGameState.renderInfo
                    gameState = { emptyGameState
                                    | players = players
                                    , renderInfo = { info | players = Just players }
                                    , resolver = 2 --auto-join
                                }
                    gameid = gameState.gameid
                    playerid = "1"
                    playerInfo = { gameid = gameid
                                 , number = 1
                                 , name = name
                                 }
                    st2 = { state
                              | gameDict =
                                  Dict.insert gameid gameState state.gameDict
                              , playeridDict =
                                  Dict.insert gameid [playerid] state.playeridDict
                              , playerInfoDict =
                                  Dict.insert playerid playerInfo
                                      state.playerInfoDict
                          }                        
                    msg = NewRsp { gameid = gameid
                                 , playerid = playerid
                                 , players = players
                                 , name = name
                                 }
                in
                    -- The non-proxy server will generate new gameid and playerid
                    (st2, msg)
            else
                ( state
                , errorRsp message IllegalPlayerCountErr "Players must be 2 or 4"
                )
        JoinReq { gameid, name } ->
            case checkGameid state message gameid JoinPhase of
                Err err ->
                    (state, err)
                Ok gameState ->
                    joinReq state gameState message gameid name
        PlaceReq { playerid, placement } ->
            case (if state.placeOnly then
                      checkOnlyPlayerid state message playerid
                  else
                      checkPlayerid state message playerid PlacementPhase
                 )
            of
                Err err ->
                        (state, err)
                Ok (gameState, number) ->
                    updateGameState state
                        <| placeReq gameState state.placeOnly
                            message placement number
        ResolveReq { playerid, resolution } ->
            case checkPlayerid state message playerid ResolutionPhase of
                Err err ->
                    (state, err)
                Ok (gameState, _) ->
                    updateGameState state
                        <| resolveReq gameState message resolution
        ResignReq { playerid } ->
            case checkOnlyPlayerid state message playerid of
                Err err ->
                    (state, err)
                Ok (gameState, number) ->
                    if List.member number gameState.resignedPlayers then
                        ( state
                        , errorRsp message IllegalRequestErr "Already resigned"
                        )
                    else
                        let resignReason = ResignationReason number
                            gameOverPhase = GameOverPhase resignReason
                            gameid = gameState.gameid
                            gameOverMessage =
                                GameOverRsp { gameid = gameid
                                            , reason = resignReason
                                            }
                            resignMessage = ResignRsp { gameid = gameid
                                                      , number = number
                                                      }
                            gameOverRes = updateGameState
                                          state ( { gameState
                                                      | phase = gameOverPhase
                                                  }
                                                , gameOverMessage
                                                )
                            resignedPlayers = adjoin
                                              number gameState.resignedPlayers
                            resignGameState = { gameState
                                                  | resignedPlayers = resignedPlayers
                                              }
                            resignRes = updateGameState
                                        state ( resignGameState
                                              , resignMessage
                                              )
                        in
                            case gameState.phase of
                                GameOverPhase _ ->
                                    ( state
                                    , errorRsp
                                        message IllegalRequestErr "Game is over"
                                    )
                                StartPhase ->
                                    gameOverRes
                                JoinPhase ->
                                    gameOverRes
                                _ ->
                                    if (gameState.players-1) <=
                                        (List.length resignedPlayers)
                                    then
                                        gameOverRes
                                    else
                                        resignRes
        -- Errors
        UndoReq { playerid, message } ->
            case checkOnlyPlayerid state message playerid of
                Err err ->
                    (state, err)
                Ok (gameState, _) ->
                    updateGameState state
                        <| undoReq gameState message
        -- Chat
        ChatReq { playerid, text } ->
            case checkOnlyPlayerid state message playerid of
                Err err ->
                    (state, err)
                Ok (gameState, number)  ->
                    ( state
                    , ChatRsp { gameid = gameState.gameid
                              , text = text
                              , number = number
                              }
                    )
        _ ->
            ( state
            , errorRsp message IllegalRequestErr "Illegal Request"
            )

updateGameState : ServerState -> (GameState, Message) -> (ServerState, Message)
updateGameState state (gameState, message) =
    ( { state
          | gameDict = Dict.insert gameState.gameid gameState state.gameDict
      }
    , message
    )

joinReq : ServerState -> GameState -> Message -> String -> String -> (ServerState, Message)
joinReq state gameState message gameid name =
    let player = gameState.resolver
        playerid = toString player --temporary in real server
        joinDone = (player == gameState.players)
        msg = JoinRsp { gameid = gameid
                      , players = gameState.players
                      , name = name
                      , playerid = Just playerid
                      , number = player
                      }
        gs2 = { gameState
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
                                  gameState.history
              }
        playerInfo = { gameid = gameid
                     , number = player
                     , name = name
                     }
        st2 = { state
                  | gameDict = Dict.insert gameid gs2 state.gameDict
                  , playerInfoDict =
                      Dict.insert playerid playerInfo state.playerInfoDict
                  , playeridDict =
                      Dict.insert gameid
                          ( playerid ::
                                ( Maybe.withDefault []
                                      <| Dict.get gameid state.playeridDict
                                )
                          )
                          state.playeridDict
              }
    in
        -- The non-proxy server will generate a new playerid
        (st2, msg)

placeReq : GameState -> Bool -> Message -> Move -> Int -> (GameState, Message)
placeReq state placeOnly message placement number =
    let placements = Dict.insert number placement state.placements
        done = placeOnly || (state.players == Dict.size placements)
        placeRsp = PlaceRsp { gameid = state.gameid, number = number }
        placementsList = if done then
                             List.map Tuple.second <| Dict.toList placements
                         else
                             []
    in
        case placement of
            Placement _ _ ->
                if placeOnly || (isLegalMove placement state.board) then
                    let plcmnts = if done then
                                      Dict.empty
                                  else
                                      placements
                        board = if done then
                                    List.foldl
                                        makeMove state.board placementsList
                                else
                                    state.board
                        renderInfo = state.renderInfo
                        (phase, unresolvedPiles, turn, resolver) =
                             if done then
                                 let displayList =
                                         computeDisplayList board renderInfo
                                 in
                                     case displayList.unresolvedPiles of
                                         [] ->
                                             ( PlacementPhase, [], state.turn+1
                                             , (state.resolver % state.players) + 1
                                             )
                                         piles ->
                                             ( ResolutionPhase, piles, state.turn
                                             , state.resolver
                                             )
                                else
                                    (PlacementPhase, [], state.turn, state.resolver)
                        his = if done then
                                  case state.history of
                                      turn :: tail ->
                                          { turn
                                              | placements = placementsList
                                          } :: tail
                                      history ->
                                          history
                              else
                                  state.history
                        history = if done &&
                                     (placeOnly || (phase == PlacementPhase))
                                  then
                                      (newTurn turn resolver) ::his
                                  else
                                      his
                        (gameOver, reason) =
                            if not done then
                                (False, TimeoutReason)
                            else if unresolvedPiles == [] then
                                case findFullHomeCircle board renderInfo of
                                    Nothing ->
                                        (False, TimeoutReason)
                                    Just player ->
                                        (True
                                        , HomeCircleFullReason
                                            player placementsList
                                        )
                            else
                                if canResolve
                                    board renderInfo (Just unresolvedPiles)
                                then
                                    (False, TimeoutReason)
                                else
                                    (True, UnresolvableReason placementsList)
                    in
                        ( { state
                              | placements = plcmnts
                              , board = board
                              , phase = phase
                              , turn = turn
                              , resolver = resolver
                              , unresolvedPiles = unresolvedPiles
                              , history = history
                          }
                        , if done then
                              if gameOver then
                                  GameOverRsp { gameid = state.gameid
                                              , reason = reason
                                              }
                              else
                                  PlacedRsp { gameid = state.gameid
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
                        [] ->
                            False
                        moves ->
                            List.member move moves
        _ ->
            False

resolveReq : GameState -> Message -> Move -> (GameState, Message)
resolveReq state message resolution =
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
                    resolveRsp = ResolveRsp { gameid = state.gameid
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

undoReq : GameState -> Message -> (GameState, Message)
undoReq state undoMessage =
    case state.history of
        [] ->
            ( state
            , errorRsp undoMessage IllegalRequestErr "No history"
            )
        turn :: tail ->
            case undoMessage of
                PlacedRsp { gameid, placements } ->
                    if turn.resolutions == [] &&
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
                            , UndoRsp { gameid = state.gameid
                                      , message = undoMessage
                                      }
                            )
                    else if turn.resolutions == [] &&
                              turn.placements == []
                    then
                        case undoReq { state | history = tail } undoMessage of
                            (_, ErrorRsp _ as err) ->
                                (state, err)
                            res ->
                                res
                    else
                        ( state
                        , errorRsp undoMessage IllegalRequestErr
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
                                            undoMessage
                                    in
                                        case response of
                                            ErrorRsp _ ->
                                                (state, response)
                                            _ ->
                                                (st2, response)
                                _ ->
                                    ( state
                                    , errorRsp undoMessage
                                        IllegalRequestErr "No matching history"
                                    )
                        else
                            ( state
                            , errorRsp undoMessage
                                IllegalRequestErr "Not resolution phase"
                            )
                    else
                        case LE.last turn.resolutions of
                            Nothing ->
                                ( state
                                , errorRsp undoMessage
                                    IllegalRequestErr "Can't happen"
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
                                    , UndoRsp { gameid = state.gameid
                                              , message = undoMessage
                                              }
                                    )
                                else
                                    ( state
                                    , errorRsp undoMessage IllegalRequestErr
                                        "No matching resolution"
                                    )
                _ ->
                    ( state
                    , errorRsp undoMessage IllegalRequestErr
                        "Can only undo placed and resolve responses."
                    )
