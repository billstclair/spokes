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
                                        , getGameList, setGameList
                                        , createRestoreState
                                        )

import Spokes.Server.EncodeDecode exposing ( encodeMessage )
import Spokes.Server.Error exposing ( ServerError(..), errnum )
import Spokes.Types as Types
    exposing ( Board, DisplayList, Move(..), StonePile, RenderInfo
             , History, newTurn, Message(..), ServerPhase(..)
             , GameState, ServerState, ServerInterface(..)
             , GameOverReason(..), RestoreState
             , PublicGames, PublicGame, emptyPublicGames, noMessage
             , butLast, adjoin
             )
import Spokes.Board exposing ( renderInfo, computeDisplayList, initialBoard
                             , getNode, isLegalMove, makeMove, undoMove
                             , canResolve, findFullHomeCircle
                             , boardToEncodedString, encodedStringToBoard
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
    , publicGames = emptyPublicGames
    }

dummyGameid : String
dummyGameid =
    "<gameid>"

emptyGameState : GameState
emptyGameState =
    { board = initialBoard
    , restoreState = Nothing
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

checkResigned : GameState -> Int -> Message -> Maybe Message
checkResigned gameState number message =
    case LE.find (\p -> p == number) gameState.resignedPlayers of
        Just _ ->
            Just <| errorRsp message IllegalRequestErr "Player has resigned"
        Nothing ->
            Nothing

processServerMessage : ServerState -> Message -> (ServerState, Message)
processServerMessage state message =
    case message of
        -- Basic game play
        NewReq { players, name, isPublic, restoreState } ->
            newReq state message players name isPublic restoreState
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
                    case checkResigned gameState number message of
                        Just err ->
                            (state, err)
                        Nothing ->
                            updateGameState state
                                <| placeReq gameState state.placeOnly
                                    message placement number
        ResolveReq { playerid, resolution } ->
            case checkPlayerid state message playerid ResolutionPhase of
                Err err ->
                    (state, err)
                Ok (gameState, number) ->
                    case checkResigned gameState number message of
                        Just err ->
                            (state, err)
                        Nothing ->
                            updateGameState state
                                <| resolveReq gameState message resolution
        ResponseCountReq { playerid, number } ->
            case checkOnlyPlayerid state message playerid of
                Err err ->
                    (state, err)
                Ok (gameState, _) ->
                    let playerInfoDict = state.playerInfoDict
                        gameid = gameState.gameid
                    in
                        case Dict.get playerid playerInfoDict of
                            Nothing ->
                                (state, noMessage)
                            Just info ->
                                if number == info.responseCount then
                                    (state, noMessage)
                                else case createRestoreState gameid state of
                                         Nothing ->
                                             (state, noMessage)
                                         Just restoreState ->
                                             ( state
                                             , ResponseCountRsp
                                                   { gameid = gameid
                                                   , number = info.responseCount
                                                   , restoreState = restoreState
                                                   -- TODO
                                                   , votedUnresolvable = []
                                                   }
                                             )                                
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
                            resignedPlayers = number :: gameState.resignedPlayers
                            gameOverRes = updateGameState
                                          state ( { gameState
                                                      | phase = gameOverPhase
                                                      , resignedPlayers =
                                                          resignedPlayers
                                                  }
                                                , gameOverMessage
                                                )
                            (gs2, resignMessage) =
                                processResign gameState gameid number resignedPlayers
                            resignRes = updateGameState
                                        state ( gs2 , resignMessage )
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
        -- Public games
        GamesReq ->
            ( state
            , GamesRsp state.publicGames
            )
        -- Errors
        UndoReq { playerid, message } ->
            case checkOnlyPlayerid state message playerid of
                Err err ->
                    (state, err)
                Ok (gameState, number) ->
                    case checkResigned gameState number message of
                        Just err ->
                            (state, err)
                        Nothing ->
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

getPlayerNames : String -> ServerState -> Maybe (List String)
getPlayerNames gameid state =
    case Dict.get gameid state.playeridDict of
        Nothing ->
            Nothing
        Just playerids ->
            Just 
                <| List.map2 (\playerid defaultName ->
                                  case Dict.get playerid state.playerInfoDict of
                                      Nothing ->
                                          defaultName
                                      Just info ->
                                          info.name
                             )
                    playerids
                    ["Player 1", "Player 2", "Player 3", "Player 4"]    

createRestoreState : String -> ServerState -> Maybe RestoreState
createRestoreState gameid state =
    case Dict.get gameid state.gameDict of
        Nothing ->
            Nothing
        Just gameState ->
            case getPlayerNames gameid state of
                Nothing ->
                    Nothing
                Just names ->
                    Just { board = boardToEncodedString gameState.board
                         , players = names
                         , resolver = gameState.resolver
                         }

getGameList : PublicGames -> Int -> List PublicGame
getGameList games players =
    case players of
        2 ->
            games.twoPlayer
        4 ->
            games.fourPlayer
        _ ->
            []

setGameList : PublicGames -> Int -> List PublicGame -> PublicGames
setGameList games players publicGames =
    case players of
        2 ->
            { games | twoPlayer = publicGames }
        4 ->
            { games | fourPlayer = publicGames }
        _ ->
            games

appendGameList : PublicGames -> Int -> PublicGame -> PublicGames    
appendGameList games players game =
    setGameList games players
        <| List.append (getGameList games players) [game]

removeGameFromList : PublicGames -> String -> Int -> PublicGames
removeGameFromList games gameid players =
    let gameList = getGameList games players
    in
        setGameList games players
            <| List.filter (\game -> game.gameid /= gameid) gameList

addPlayerToPublicGame : PublicGames -> String -> Int -> String -> PublicGames
addPlayerToPublicGame games gameid players name =
    let gameList = getGameList games players
    in
        case LE.find (\game -> game.gameid == gameid) gameList of
            Nothing ->
                games
            Just game ->
                setGameList games players
                    <| { game
                           | playerNames =
                               List.append game.playerNames [name]
                       }
                        :: (List.filter (\game -> game.gameid /= gameid) gameList)

maximumPublicGames : Int
maximumPublicGames =
    10

newReq : ServerState -> Message -> Int -> String -> Bool -> Maybe RestoreState -> (ServerState, Message)
newReq state message players name isPublic restoreState =
    if not (players == 2 || players == 4) then
        ( state
        , errorRsp message IllegalPlayerCountErr "Players must be 2 or 4"
        )
    else
        if isPublic then
            let list = getGameList state.publicGames players
            in
                if (List.length list) >= maximumPublicGames then
                    ( state
                    , errorRsp message TooManyPublicGamesErr
                        "There are already too many public games."
                    )
                else
                    newReqInternal state message players name isPublic restoreState
        else
            newReqInternal state message players name isPublic restoreState

newReqInternal : ServerState -> Message -> Int -> String -> Bool -> Maybe RestoreState -> (ServerState, Message)
newReqInternal state message players name isPublic restoreState =
    let info = emptyGameState.renderInfo
        gameState = { emptyGameState
                        | board = case restoreState of
                                      Nothing ->
                                          initialBoard
                                      Just { board } ->
                                          encodedStringToBoard board
                        , restoreState = restoreState
                        , players = players
                        , renderInfo = { info | players = Just players }
                        , resolver = 2 --auto-join
                    }
        gameid = gameState.gameid
        playerid = "1"
        playerInfo = { gameid = gameid
                     , number = 1
                     , name = name
                     , responseCount = 0
                     }
        st2 = { state
                  | gameDict =
                      Dict.insert gameid gameState state.gameDict
                  , playeridDict =
                      Dict.insert gameid [playerid] state.playeridDict
                  , playerInfoDict =
                      Dict.insert playerid playerInfo
                          state.playerInfoDict
                  , publicGames =
                    if isPublic then
                        appendGameList state.publicGames players
                            <| { gameid = gameid
                               , players = players
                               , playerNames = [name]
                               }
                    else
                        state.publicGames                        
              }                        
        msg = NewRsp { gameid = gameid
                     , playerid = playerid
                     , players = players
                     , name = name
                     , restoreState = restoreState
                     }
    in
        -- The non-proxy server will generate new gameid and playerid
        (st2, msg)

processResign : GameState -> String -> Int -> List Int -> (GameState, Message)
processResign state gameid number resignedPlayers =
    let resignMessage = ResignRsp { gameid = gameid
                                  , number = number
                                  , placements = Nothing
                                  }
        state2 = { state
                     | resignedPlayers = resignedPlayers
                 }
        phase = state.phase
        resolver = state2.resolver
        (resolver2, history) =
            if ((phase == ResolutionPhase) || (phase == PlacementPhase)) &&
               (number == resolver)
            then
                let r2 = nextResolver state2
                in
                    ( r2
                    , case state2.history of
                          turn :: tail ->
                              { turn | resolver = r2 } :: tail
                          h ->
                              h
                    )
            else
                (resolver, state2.history)
    in
        if phase /= PlacementPhase then
            (state2, resignMessage)
        else
            -- It would be lovely to merge this with the placeReq() code
            let placements = state2.placements
                players = state2.players - (List.length resignedPlayers)
                done = (players == Dict.size placements)
                placementsList = if done then
                                     List.map Tuple.second
                                         <| Dict.toList placements
                                 else
                                     []
                plcmnts = if done then
                              Dict.empty
                          else
                              placements
                board = if done then
                            List.foldl makeMove state2.board placementsList
                        else
                            state2.board
                info = state2.renderInfo
                (phase2, unresolvedPiles, turn, resolver3) =
                    if done then
                        let displayList = computeDisplayList board info
                        in
                            case displayList.unresolvedPiles of
                                [] ->
                                    ( PlacementPhase, [], state2.turn+1
                                    , if number /= resolver then
                                          nextResolver state2
                                      else
                                          resolver2
                                    )
                                piles ->
                                    ( ResolutionPhase, piles, state2.turn
                                    , resolver2
                                    )
                    else
                        (PlacementPhase, [], state2.turn, resolver2)
                his = if done then
                          case history of
                              turn :: tail ->
                                  { turn | placements = placementsList } ::
                                      tail
                              h ->
                                  h
                      else
                          history
                history2 = (newTurn turn resolver3) :: his
                (response, phase3) =
                    if not done then
                        (resignMessage, phase2)
                    else
                        let placedRsp = ResignRsp
                                        { gameid = gameid
                                        , number = number
                                        , placements = Just placementsList
                                        }
                        in
                            maybeGameOver board info gameid
                                placementsList unresolvedPiles
                                placedRsp phase2
            in
                ( { state2
                      | placements = plcmnts
                      , board = board
                      , phase = phase3
                      , turn = turn
                      , resolver = resolver3
                      , unresolvedPiles = unresolvedPiles
                      , history = history2
                  }
                , response
                )

updateGameState : ServerState -> (GameState, Message) -> (ServerState, Message)
updateGameState state (gameState, message) =
    ( { state
          | gameDict = Dict.insert gameState.gameid gameState state.gameDict
          , publicGames =
              case message of
                  GameOverRsp { gameid } ->
                      removeGameFromList state.publicGames gameid gameState.players
                  _ ->
                      state.publicGames
      }
    , message
    )

joinReq : ServerState -> GameState -> Message -> String -> String -> (ServerState, Message)
joinReq state gameState message gameid name =
    let player = gameState.resolver
        playerid = toString player --temporary in real server
        players = gameState.players
        joinDone = (player == players)
        restoreState = gameState.restoreState
        unresolvedPiles = if not joinDone then
                              []
                          else
                              case restoreState of
                                  Nothing ->
                                      []
                                  Just rs ->
                                      .unresolvedPiles
                                      <| computeDisplayList
                                          gameState.board gameState.renderInfo
        msg = JoinRsp { gameid = gameid
                      , players = players
                      , name = name
                      , playerid = Just playerid
                      , number = player
                      , restoreState = restoreState
                      }
        gs2 = { gameState
                  | resolver = if joinDone then
                                   1
                               else
                                   player + 1
                  , unresolvedPiles = unresolvedPiles
                  , phase = if not joinDone then
                                JoinPhase
                            else if unresolvedPiles == [] then
                                PlacementPhase
                            else
                                ResolutionPhase
                  , history = if joinDone then
                                  [ { number = 1
                                    , resolver = case restoreState of
                                                     Nothing ->
                                                         1
                                                     Just rs ->
                                                         rs.resolver
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
                     , responseCount = 0
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
                  , publicGames =
                      if joinDone then
                          removeGameFromList state.publicGames gameid players
                      else
                          addPlayerToPublicGame
                              state.publicGames gameid players name
              }
    in
        -- The non-proxy server will generate a new playerid
        (st2, msg)

maybeGameOver : Board -> RenderInfo -> String -> List Move -> List StonePile -> Message -> ServerPhase -> (Message, ServerPhase)
maybeGameOver board renderInfo gameid moves unresolvedPiles response phase =
    if unresolvedPiles == [] then
        case findFullHomeCircle board renderInfo of
            Nothing ->
                (response, phase)
            Just player ->
                let reason = HomeCircleFullReason player moves
                in
                    ( GameOverRsp { gameid = gameid
                                  , reason = reason
                                  }
                    , GameOverPhase reason
                    )
    else if canResolve board renderInfo (Just unresolvedPiles) then
        (response, phase)
    else
        let reason = UnresolvableReason moves
        in
            ( GameOverRsp { gameid = gameid
                          , reason = reason
                          }
            , GameOverPhase reason
            )

nextResolver : GameState -> Int
nextResolver gameState =
    let resigned = gameState.resignedPlayers
        initialResolver = gameState.resolver
        players = gameState.players
        loop = (\resolver ->
                    let r = (resolver % players) + 1
                    in
                        if List.member r resigned then
                            if r == initialResolver then
                                r --shouldn't happen
                            else
                                loop r
                        else
                            r
               )
    in
        loop initialResolver

placeReq : GameState -> Bool -> Message -> Move -> Int -> (GameState, Message)
placeReq state placeOnly message placement number =
    let placements = Dict.insert number placement state.placements
        players = state.players - (List.length state.resignedPlayers)
        done = placeOnly || (players == Dict.size placements)
        gameid = state.gameid
        placeRsp = PlaceRsp { gameid = gameid, number = number }
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
                        info = state.renderInfo
                        (phase, unresolvedPiles, turn, resolver) =
                             if done then
                                 let displayList =
                                         computeDisplayList board info
                                 in
                                     case displayList.unresolvedPiles of
                                         [] ->
                                             ( PlacementPhase, [], state.turn+1
                                             , nextResolver state
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
                        (response, phase2) =
                            if not done then
                                (placeRsp, phase)
                            else
                                let placedRsp =
                                        PlacedRsp { gameid = gameid
                                                  , placements = placementsList
                                                  }
                                in
                                    maybeGameOver board info gameid
                                        placementsList unresolvedPiles
                                        placedRsp phase
                    in
                        ( { state
                              | placements = plcmnts
                              , board = board
                              , phase = phase2
                              , turn = turn
                              , resolver = resolver
                              , unresolvedPiles = unresolvedPiles
                              , history = history
                          }
                        , response
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
                    renderInfo = state.renderInfo
                    displayList = computeDisplayList board renderInfo
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
                                   nextResolver state
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
                    gameid = state.gameid
                    resolveRsp = ResolveRsp { gameid = gameid
                                            , resolution = resolution
                                            }
                    (response, phase2) =
                        maybeGameOver board renderInfo gameid
                            [resolution] unresolvedPiles resolveRsp phase
                in
                    ( { state
                          | board = board
                          , phase = phase2
                          , unresolvedPiles = unresolvedPiles
                          , turn = turn
                          , resolver = resolver
                          , history = history
                      }
                    , response
                    )
        _ ->
            ( state
            , errorRsp message IllegalRequestErr "Non-resolution move"
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
                                          , phase = ResolutionPhase
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
