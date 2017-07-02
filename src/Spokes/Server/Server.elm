port module Server exposing (..)

import Spokes.Server.EncodeDecode exposing ( encodeMessage, decodeMessage )
import Spokes.Server.Interface exposing ( emptyServerState, send
                                        , processServerMessage
                                        , getGameList, setGameList
                                        )
import Spokes.Server.Error exposing ( ServerError(..), errnum )
import Spokes.Types exposing ( Message(..), ServerState, PublicGames
                             , noMessage
                             )

import Platform exposing ( Program )
import Json.Decode as Decode exposing ( Decoder )
import Json.Encode as Encode
import Task
import Time exposing ( Time )
import Random exposing ( Seed, Generator ) 
import Char
import Dict exposing ( Dict )
import List.Extra as LE
import Debug exposing ( log )
import WebSocketServer as WSS exposing ( Socket )

main : Program Never Model Msg
main =
  Platform.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

-- PORTS

port inputPort : (Decode.Value -> msg) -> Sub msg
port outputPort : Encode.Value -> Cmd msg

-- MODEL

type alias Model =
    { state : ServerState
    , gameidDict : Dict Socket String --Socket -> gameid
    , socketsDict : Dict String (List Socket) -- gameid -> List Socket
    , seed : Seed
    }

lowercaseLetter : Generator Char
lowercaseLetter =
    Random.map (\n -> Char.fromCode (n + 97)) (Random.int 0 25)

gameidLength : Int
gameidLength =
    16                          --(log (expt 26 16) 2) -> 75

gameidGenerator : Generator String
gameidGenerator =
    Random.map String.fromList
        <| Random.list gameidLength lowercaseLetter

newGameid : Model -> (String, Model)
newGameid model =
    let (res, seed) = Random.step gameidGenerator model.seed
    in
        ( res
        , { model | seed = seed }
        )

newPlayerid : Model -> (String, Model)
newPlayerid model =
    let (id, model2) = newGameid model
    in
        ("P" ++ id, model2)

init : ( Model, Cmd Msg )
init =
    ( { state = emptyServerState
      , gameidDict = Dict.empty
      , socketsDict = Dict.empty
      , seed = Random.initialSeed 0
      }
    , Task.perform Tick Time.now
    )

-- UPDATE

type Msg
  = Connection WSS.Socket
  | Disconnection WSS.Socket
  | SocketMessage Socket String
  | Tick Time
  | Noop

update : Msg -> Model -> (Model, Cmd Msg)
update message model =
  case (Debug.log "Msg" message) of
    Connection socket ->
      ( model, Cmd.none )
    Disconnection socket ->
        disconnection model socket
    SocketMessage socket message ->
        socketMessage model socket message
    Tick time ->
        let seed = Random.initialSeed <| round time
        in
            ( { model | seed = seed }
            , Cmd.none
            )
    Noop -> (model, Cmd.none)

removeFromPublicGames : ServerState -> String -> ServerState
removeFromPublicGames state gameid =
    let games = state.publicGames
        filter = (\game -> game.gameid /= gameid)
    in
        { state
            | publicGames =
              { twoPlayer = List.filter filter games.twoPlayer
              , fourPlayer = List.filter filter games.fourPlayer
              }
        }

killGame : Model -> String -> Model
killGame model gameid =
    let state = removeFromPublicGames model.state gameid
        playerInfoDict = state.playerInfoDict
        playeridDict = state.playeridDict
        gameDict = state.gameDict
        playerids = Dict.get (log "killGame" gameid) playeridDict
    in
        { model |
              state = { state
                          | playerInfoDict =
                              case log "  playerids" playerids of
                                  Nothing ->
                                      playerInfoDict
                                  Just ids ->
                                      List.foldl (\k d -> Dict.remove k d)
                                          playerInfoDict ids
                          , playeridDict = Dict.remove gameid playeridDict
                          , gameDict = Dict.remove gameid gameDict
                      }
        }

disconnection : Model -> Socket -> (Model, Cmd Msg)
disconnection model socket =
    case Dict.get socket model.gameidDict of
        Nothing ->
            (model, Cmd.none)
        Just gameid ->
            let model2 = { model | gameidDict = Dict.remove socket model.gameidDict }
                socketsDict = model.socketsDict
            in
                case Dict.get gameid model.socketsDict of
                    Nothing ->
                        ( model2, Cmd.none )
                    Just sockets ->
                        let socks = List.filter (\s -> s /= socket) sockets
                        in
                            ( if socks == [] then
                                  killGame { model2
                                               | socketsDict =
                                                   Dict.remove gameid socketsDict
                                           }
                                      gameid
                              else
                                  { model2
                                      | socketsDict =
                                          Dict.insert gameid socks socketsDict
                                  }
                            , Cmd.none
                            )

sendToOne : Message -> Socket -> Cmd Msg
sendToOne message socket =
    WSS.sendToOne outputPort
        (log "send" (encodeMessage message))
        (log "  " socket)

sendToMany : Message -> (List Socket) -> Cmd Msg
sendToMany message sockets =
    WSS.sendToMany outputPort
        (log "send" (encodeMessage message))
        (log "  " sockets)
        |> Cmd.batch

socketMessage : Model -> Socket -> String -> (Model, Cmd Msg)
socketMessage model socket request =
    case decodeMessage request of
        Err msg ->
            let response = ErrorRsp { request = request
                                    , id = errnum UnparsableJsonErr
                                    , text = "Can't parse request"
                                    }
            in
                ( model
                , sendToOne response socket
                )
        Ok message ->
            let (state, response) = processServerMessage model.state message
            in
                if response == noMessage then
                    ( { model | state = state }
                    , Cmd.none
                    )
                else
                    processResponse model socket state response

updatePublicGameId : PublicGames -> Int -> String -> String -> PublicGames
updatePublicGameId games players gameid gid =
    let gameList = getGameList games players
    in
        case LE.find (\game -> game.gameid == gameid) gameList of
            Nothing ->
                games
            Just game ->
                setGameList games players
                    <| { game | gameid = gid }
                        :: (List.filter (\game -> game.gameid /= gameid) gameList)

processResponse : Model -> Socket -> ServerState -> Message -> (Model, Cmd Msg)
processResponse model socket state response =
    case response of
        (NewRsp { gameid, playerid, players, name, restoreState }) ->
            let (model2, _) = disconnection model socket
                (gid, model3) = newGameid model2
                (pid, model4) = newPlayerid model3
                state2 = case Dict.get gameid state.gameDict of
                             Nothing ->
                                 state --can't happen
                             Just gs ->
                                 let gs2 = { gs | gameid = gid }
                                     gameDict =
                                         Dict.remove gameid state.gameDict
                                     playerInfoDict =
                                         Dict.remove playerid state.playerInfoDict
                                     playerInfo =
                                         { gameid = gid
                                         , number = 1
                                         , name = name
                                         , responseCount = 1
                                         }
                                     playeridDict =
                                         Dict.remove gameid state.playeridDict
                                 in
                                     { state
                                         | gameDict = Dict.insert gid gs2 gameDict
                                         , playerInfoDict =
                                             Dict.insert pid playerInfo
                                                 playerInfoDict
                                         , playeridDict =
                                             Dict.insert gid [pid] playeridDict
                                         , publicGames =
                                             updatePublicGameId state.publicGames
                                                 gs.players gameid gid
                                     }
                model5 = { model4
                             | state = state2
                             , gameidDict =
                                 Dict.insert socket gid model4.gameidDict
                             , socketsDict =
                                 Dict.insert gid [socket] model4.socketsDict
                         }
                response = NewRsp { gameid = gid
                                  , playerid = pid
                                  , players = players
                                  , name = name
                                  , restoreState = restoreState
                                  }
            in
                ( model5
                , sendToOne response socket
                )
        JoinRsp { gameid, players, name, playerid, number, restoreState } ->
             let (model2, _) = disconnection model socket
                 (pid, model3) = newPlayerid model2
                 playerInfo = { gameid = gameid
                              , number = number
                              , name = name
                              , responseCount = 0
                              }
                 playerInfoDict = case playerid of
                                  Just id -> Dict.remove id state.playerInfoDict
                                  Nothing -> state.playerInfoDict
                 playerids = case Dict.get gameid state.playeridDict of
                                 Nothing -> [] --can't happen
                                 Just ids -> ids
                 newPlayerids = case playerid of
                                    Nothing ->
                                        playerids
                                    Just id ->
                                        pid ::
                                        (List.filter (\i -> i /= id) playerids)
                 st2 = { state
                           | playerInfoDict =
                               Dict.insert pid playerInfo playerInfoDict
                           , playeridDict =
                               case playerid of
                                   Nothing -> state.playeridDict
                                   Just id -> 
                                       Dict.insert gameid newPlayerids
                                           state.playeridDict
                       }
                 sockets = case Dict.get gameid model3.socketsDict of
                               Nothing -> [] --can't happen
                               Just socks -> socks
                 model4 = { model3
                              | state = st2
                              , gameidDict =
                                  Dict.insert socket gameid model3.gameidDict
                              , socketsDict =
                                  Dict.insert gameid (socket :: sockets)
                                      model3.socketsDict
                          }
                 rsp = JoinRsp { gameid = gameid
                               , players = players
                               , name = name
                               , playerid = Just pid
                               , number = number
                               , restoreState = restoreState
                               }
                 rsp2 = JoinRsp { gameid = gameid
                                , players = players
                                , name = name
                                , playerid = Nothing
                                , number = number
                                , restoreState = Nothing
                                }
                 oldJoins = List.concatMap
                            (\pid -> case Dict.get pid st2.playerInfoDict of
                                         Nothing ->
                                             []
                                         Just { number, name } ->
                                             [ JoinRsp { gameid = gameid
                                                       , players = players
                                                       , name = name
                                                       , playerid = Nothing
                                                       , number = number
                                                       , restoreState = Nothing
                                                       }
                                             ]
                            )
                            playerids
             in
                 ( bumpResponseCount (bumpAllResponseCounts model4 gameid)
                       pid (List.length oldJoins)
                 , Cmd.batch
                     [ sendToOne rsp socket
                     , List.map (\r -> sendToOne r socket) oldJoins
                         |> Cmd.batch
                     , sendToMany rsp2 sockets
                     ]
                 )
        ErrorRsp _ ->
            ( model
            , sendToOne response socket
            )
        _ ->
            let (gameid, model2) =
                    case Dict.get socket model.gameidDict of
                        Just gid ->
                            (gid, model)
                        Nothing ->
                            let gid = responseGameid response
                            in
                                if gid == "" then
                                    (gid, model)
                                else
                                    let socks =
                                        case Dict.get gid model.socketsDict of
                                            Nothing -> [socket]
                                            Just ss -> socket :: ss
                                    in
                                        ( gid
                                        , { model
                                              | gameidDict =
                                                  Dict.insert socket gid
                                                      model.gameidDict
                                              , socketsDict =
                                                  Dict.insert gid socks
                                                      model.socketsDict
                                          }
                                        )
                sockets = case Dict.get gameid model2.socketsDict of
                              Nothing ->
                                  [socket] --can't happen
                              Just socks ->
                                  socks
            in
                ( bumpAllResponseCounts { model2 | state = state } gameid
                , sendToMany response sockets
                )

responseGameid : Message -> String
responseGameid message =
    case message of
        PlaceRsp { gameid } -> gameid
        ResolveRsp { gameid } -> gameid
        UndoRsp { gameid } -> gameid
        ChatRsp { gameid } -> gameid
        NewRsp { gameid } -> gameid
        JoinRsp { gameid } -> gameid
        _ -> ""

bumpResponseCount : Model -> String -> Int -> Model
bumpResponseCount model playerid count =
    if count <= 0 then
        model
    else
        let state = model.state
            playerInfoDict = state.playerInfoDict
        in
            case Dict.get playerid playerInfoDict of
                Nothing ->
                    model
                Just playerInfo ->
                    { model
                        | state =
                          { state
                              | playerInfoDict =
                                  Dict.insert playerid
                                      { playerInfo
                                          | responseCount =
                                              playerInfo.responseCount + 1
                                      }
                                      playerInfoDict
                          }
                    }

bumpAllResponseCounts : Model -> String -> Model
bumpAllResponseCounts model gameid =
    let state = model.state
    in
        case Dict.get gameid state.playeridDict of
            Nothing ->
                model
            Just playerids ->
                { model
                    | state =
                        { state
                            | playerInfoDict =
                                List.foldl (\pid dict ->
                                                case Dict.get pid dict of
                                                    Nothing ->
                                                        dict
                                                    Just info ->
                                                        Dict.insert pid
                                                            { info |
                                                                  responseCount =
                                                                  info.responseCount + 1
                                                            }
                                                            dict
                                           )
                                    state.playerInfoDict
                                    playerids
                        }
                }                    

-- SUBSCRIPTIONS

decodeMsg : Decode.Value -> Msg
decodeMsg value =
  let
    decoder = WSS.eventDecoder
      { onConnection = (\socket _ -> Connection socket)
      , onDisconnection = (\socket _ -> Disconnection socket)
      , onMessage = (\socket _ message -> SocketMessage socket message)
      }
  in
    Decode.decodeValue decoder value
      |> Result.withDefault Noop

subscriptions : Model -> Sub Msg
subscriptions model =
    inputPort decodeMsg
