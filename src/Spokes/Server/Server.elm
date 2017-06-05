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
    { stateDict : Dict Gameid ServerState
    , socketDict : Dict Gameid (List Socket)
    , gameidDict : Dict Socket (Gameid, Int)
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

type alias Gameid =
    String

newGameid : Model -> (Gameid, Model)
newGameid model =
    let (res, seed) = Random.step gameidGenerator model.seed
    in
        ( res
        , { model | seed = seed }
        )

init : ( Model, Cmd Msg )
init =
    ( { stateDict = Dict.empty
      , socketDict = Dict.empty
      , gameidDict = Dict.empty
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
      ( model
      , Cmd.none
      )
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
    let (socketDict, stateDict) =
            case Dict.get socket model.gameidDict of
                Nothing ->
                    (model.socketDict, model.stateDict)
                Just (gameid, _) ->
                    let sckd = model.socketDict
                    in
                        case Dict.get gameid sckd of
                            Nothing ->
                                (sckd, model.stateDict)
                            Just sockets ->
                                let socks = List.filter (\s -> s /= socket)
                                            sockets
                                in
                                    if socks == [] then
                                        ( Dict.remove gameid sckd
                                        , Dict.remove gameid model.stateDict
                                        )
                                    else
                                        ( Dict.insert gameid socks sckd
                                        , model.stateDict
                                        )                                 
    in
        ( { model
              | gameidDict = Dict.remove socket model.gameidDict
              , socketDict = socketDict
              , stateDict = stateDict
          }
        , Cmd.none
        )

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
                , WSS.sendToOne outputPort (encodeMessage response) socket
                )
        Ok message ->
            case checkPlayerNumber socket model message of
                Just errrsp ->
                    ( model
                    , WSS.sendToOne outputPort (encodeMessage errrsp) socket
                    )
                Nothing ->
                    let state = case Dict.get socket model.gameidDict of
                                    Just (gameid, _) ->
                                        Maybe.withDefault emptyServerState
                                            <| Dict.get gameid model.stateDict
                                    _ ->
                                        emptyServerState
                        (state2, response) = processServerMessage state message
                    in
                        processResponse model socket state2 response

getSocketGameid : Socket -> Model -> Maybe (Gameid, Int)
getSocketGameid socket model =
    Dict.get socket model.gameidDict

getSocketState : Socket -> Model -> Maybe ServerState
getSocketState socket model =
    case getSocketGameid socket model of
        Nothing ->
            Nothing
        Just (gameid, _) ->
            Dict.get gameid model.stateDict

wrongPlayerNumber : Int -> Message -> Message
wrongPlayerNumber number message =
    ErrorRsp { request = encodeMessage message
             , id = errnum WrongPlayerNumberErr
             , text = "Player number should be: " ++ (toString number)
             }

checkPlayerNumber : Socket -> Model -> Message -> Maybe Message
checkPlayerNumber socket model message =
    case (case message of
              PlaceReq { number } ->
                  Just number
              ChatReq { number } ->
                  Just number
              ResolveReq _ ->
                  case getSocketState socket model of
                      Nothing ->
                          Nothing
                      Just state ->
                          Just state.resolver
              _ ->
                  Nothing
         )
    of
        Nothing ->
            Nothing
        Just number ->
            case getSocketGameid socket model of
                Nothing ->
                    Just <| wrongPlayerNumber number message
                Just (_, was) ->
                    if number /= was then
                        Just <| wrongPlayerNumber number message
                    else
                        Nothing              

processResponse : Model -> Socket -> ServerState -> Message -> (Model, Cmd Msg)
processResponse model socket state response =
    case response of
        (NewRsp { players, name }) ->
            let (gameid, model2) = newGameid model
                state2 = { state | gameid = gameid }
                model3 = { model2
                             | gameidDict =
                               Dict.insert socket (gameid, 1) model2.gameidDict
                             , socketDict =
                               Dict.insert gameid [socket] model2.socketDict
                             , stateDict =
                               Dict.insert gameid state2 model2.stateDict
                         }
                response = NewRsp { gameid = gameid
                                  , players = players
                                  , name = name
                                  }
            in
                ( model3
                , WSS.sendToOne outputPort (encodeMessage response) socket
                )
        JoinRsp { gameid, number } ->
             let sockets = case Dict.get gameid model.socketDict of
                               Nothing ->
                                   [socket] --can't happen
                               Just socks ->
                                   socket :: socks
                 model2 = { model
                              | gameidDict =
                                Dict.insert socket (gameid, number) model.gameidDict
                              , socketDict =
                                Dict.insert gameid sockets model.socketDict
                              , stateDict =
                                Dict.insert gameid state model.stateDict
                          }
             in
                 ( model2
                 , WSS.sendToMany outputPort (encodeMessage response) sockets
                     |> Cmd.batch
                 )
        _ ->
            let (model2, sockets) =
                    case getSocketGameid socket model of
                        Nothing ->
                            (model, [socket])
                        Just (gameid, _) ->
                            ( { model | stateDict =
                                    Dict.insert gameid state model.stateDict
                              }
                            , case Dict.get gameid model.socketDict of
                                  Nothing ->
                                      [socket]
                                  Just socks ->
                                      socks
                            )
            in
                ( model2
                , WSS.sendToMany outputPort (encodeMessage response) sockets
                    |> Cmd.batch
                )

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
