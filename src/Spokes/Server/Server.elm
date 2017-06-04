port module Server exposing (..)

import Spokes.Server.EncodeDecode exposing ( encodeMessage, decodeMessage )
import Spokes.Server.Interface exposing ( emptyServerState, send
                                        , processServerMessage
                                        )
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
    , gameidDict : Dict Socket Gameid
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
        let (socketDict, stateDict) =
                case Dict.get socket model.gameidDict of
                    Nothing ->
                        (model.socketDict, model.stateDict)
                    Just gameid ->
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
    SocketMessage socket message ->
        socketMessage model socket message
    Tick time ->
        let seed = Random.initialSeed <| round time
        in
            ( { model | seed = seed }
            , Cmd.none
            )
    Noop -> (model, Cmd.none)

socketMessage : Model -> Socket -> String -> (Model, Cmd Msg)
socketMessage model socket message =
      ( model
      , Cmd.none
--      , WSS.sendToMany outputPort message model.sockets
--          |> Cmd.batch
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
