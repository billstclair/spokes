port module Server exposing (..)

import Spokes.Server.EncodeDecode exposing ( encodeMessage, decodeMessage )
import Spokes.Server.Interface exposing ( emptyServerState, send
                                        , processServerMessage
                                        )
import Spokes.Server.Error exposing ( ServerError(..), errnum )
import Spokes.Types exposing ( Message(..), ServerState )

import Platform exposing ( Program )
import Json.Decode as Decode exposing ( Decoder )
import Json.Encode as Encode
import Task
import Time exposing ( Time )
import Random exposing ( Seed, Generator ) 
import Char
import Dict exposing ( Dict )
import Debug exposing ( log )
import WebSocketServer as WSS exposing ( Socket, sendToOne, sendToMany )

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

disconnection : Model -> Socket -> (Model, Cmd Msg)
disconnection model socket =
    case Dict.get socket model.gameidDict of
        Nothing ->
            (model, Cmd.none)
        Just gameid ->
            let model2 = { model | gameidDict = Dict.remove socket model.gameidDict }
            in
                case Dict.get gameid model.socketsDict of
                    Nothing ->
                        ( model2, Cmd.none )
                    Just sockets ->
                        let socks = List.filter (\s -> s /= socket) sockets
                        in
                            ( { model2
                                  | socketsDict =
                                    Dict.insert gameid socks model.socketsDict
                              }
                            , Cmd.none
                            )

sendToOne : Message -> Socket -> Cmd Msg
sendToOne message socket =
    WSS.sendToOne outputPort (encodeMessage message) socket

sendToMany : Message -> (List Socket) -> Cmd Msg
sendToMany message sockets =
    WSS.sendToMany outputPort (encodeMessage message) sockets
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
                processResponse model socket state response

processResponse : Model -> Socket -> ServerState -> Message -> (Model, Cmd Msg)
processResponse model socket state response =
    case response of
        (NewRsp { gameid, playerid, players, name }) ->
            let (gid, model2) = newGameid model
                (pid, model3) = newPlayerid model2
                gameState = Dict.get gameid state.gameDict
                state2 = case gameState of
                             Nothing ->
                                 state --can't happen
                             Just gs ->
                                 let gs2 = { gs | gameid = gid }
                                     gameDict =
                                         Dict.remove gameid state.gameDict
                                     gameidDict =
                                         Dict.remove playerid state.gameidDict
                                     playeridDict =
                                         Dict.remove gameid state.playeridDict
                                 in
                                     { state
                                         | gameDict = Dict.insert gid gs2 gameDict
                                         , gameidDict =
                                             Dict.insert pid (gid, 1) gameidDict
                                         , playeridDict =
                                             Dict.insert gid [pid] playeridDict
                                     }
                model4 = { model3
                             | state = state2
                             , gameidDict =
                                 Dict.insert socket gid model3.gameidDict
                             , socketsDict =
                                 Dict.insert gid [socket] model3.socketsDict
                         }
                response = NewRsp { gameid = gid
                                  , playerid = pid
                                  , players = players
                                  , name = name
                                  }
            in
                ( model4
                , sendToOne response socket
                )
        JoinRsp { gameid, name, playerid, number } ->
             let (pid, model2) = newPlayerid model
                 gameidDict = case playerid of
                                  Just id -> Dict.remove id state.gameidDict
                                  Nothing -> state.gameidDict
                 playerids = case Dict.get gameid state.playeridDict of
                                 Nothing -> [pid] --can't happen
                                 Just ids ->
                                     pid ::
                                         (case playerid of
                                              Nothing -> ids
                                              Just id ->
                                                  (List.filter (\i -> id /= i) ids)
                                         )
                 st2 = { state
                           | gameidDict =
                               Dict.insert pid (gameid, number) gameidDict
                           , playeridDict =
                               Dict.insert gameid playerids state.playeridDict
                       }
                 sockets = case Dict.get gameid model.socketsDict of
                               Nothing -> [socket] --can't happen
                               Just socks -> socket :: socks
                 model3 = { model2
                              | state = st2
                              , gameidDict =
                                  Dict.insert socket gameid model2.gameidDict
                              , socketsDict =
                                  Dict.insert gameid sockets model2.socketsDict
                          }
                 rsp = JoinRsp { gameid = gameid
                               , name = name
                               , playerid = Just pid
                               , number = number
                               }
                 rsp2 = JoinRsp { gameid = gameid
                                , name = name
                                , playerid = Nothing
                                , number = number
                                }
             in
                 ( model3
                 , Cmd.batch
                     [ sendToOne rsp socket
                     , sendToMany rsp2
                          <| List.filter (\s -> s /= socket) sockets
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
                                  [socket]
                              Just socks ->
                                  socks
            in
                ( model2
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
