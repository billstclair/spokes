----------------------------------------------------------------------
--
-- Spokes.elm
-- Chris St. Clair's Spokes board game, in Elm.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Spokes exposing (..)

import Spokes.Types as Types exposing ( Page(..), Msg(..), Board, RenderInfo
                                      , Move(..), MovedStone(..)
                                      , DisplayList, emptyDisplayList
                                      , StonePile, Color(..)
                                      , Turn, History, newTurn
                                      , ServerPhase(..), ServerInterface(..)
                                      , Message(..)
                                      , GameOverReason(..), RestoreState
                                      , PublicGames, PublicGame, emptyPublicGames
                                      , movedStoneString, butLast, adjoin
                                      )
import Spokes.Board as Board exposing ( render, isLegalPlacement, makeMove
                                      , computeDisplayList, findResolution
                                      , placementText, colorLetter
                                      )
import Spokes.Server.EncodeDecode exposing ( decodeMessage
                                           , encodeRestoreState, decodeRestoreState
                                           )
import Spokes.Server.Interface as Interface exposing
    ( makeProxyServer, makeServer )

import Html exposing ( Html, Attribute
                     , div, text, span, p, h2, h3, a, node
                     , input, table, tr, th, td, button
                     , textarea
                     )
import Html.Attributes exposing ( value, size, maxlength, href, src, title
                                , alt, style, selected, type_, name, checked
                                , placeholder, disabled, target
                                , width, height, class
                                , readonly, id
                                )
import Html.Events exposing ( onClick, onInput, onFocus, onCheck, on, keyCode )
import Array exposing ( Array )
import Char
import List.Extra as LE
import WebSocket
import Http
import Json.Decode as Json
import Dom.Scroll as Scroll
import Task
import Set
import Time exposing ( Time )
import Debug exposing ( log )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

type alias Model =
    { page : Page
    , history : History
    , board : Board
    , renderInfo : RenderInfo
    , displayList : DisplayList
    , players : Int
    , newPlayers : Int
    , playerNames : List (Int, String)
    , resignedPlayers : List Int
    , turn : Int
    , phase : ServerPhase
    , lastFocus : Int
    , inputColor : Color
    , inputs : Array String
    , placement : Maybe Move
    , resolver : Int
    , selectedPile : Maybe StonePile
    , server : ServerInterface Msg
    , connected : Bool
    , localServer : Maybe (ServerInterface Msg)
    , responseCount : Int
    , time : Time
    , nextResponseCountTime : Maybe Time
    , gameid : String
    , placeOnly : Bool
    , restoreState : String
    , playerNumber : Int
    , playerid : String
    , serverUrl : String
    , isLocal : Bool
    , newIsLocal : Bool
    , isPublic : Bool
    , name : String
    , newGameid : String
    , publicGames : PublicGames
    , chat : String
    , chatInput : String
    , chatScroll : Float
    }

placeOnly : Model -> Bool
placeOnly model =
    model.isLocal && model.placeOnly

initialInputs : Array String
initialInputs = Array.repeat 4 ""

initialModel : Model
initialModel =
    { page = GamePage
    , history = [ newTurn 1 1 ]
    , board = Board.initialBoard
    , renderInfo = Board.renderInfo 600
    , displayList = emptyDisplayList
    , players = 2
    , newPlayers = 2
    , playerNames = []
    , resignedPlayers = []
    , turn = 1
    , phase = StartPhase
    , lastFocus = 1
    , inputColor = White
    , inputs = initialInputs
    , placement = Nothing
    , resolver = 1
    , selectedPile = Nothing
    , server = makeProxyServer ServerResponse
    , connected = True
    , localServer = Nothing
    , responseCount = 0
    , time = 0
    , nextResponseCountTime = Nothing
    , gameid = ""
    , placeOnly = False
    , restoreState = ""
    , playerNumber = 1
    , playerid = ""
    , serverUrl = "ws://localhost:8080"
    , isLocal = True
    , newIsLocal = False
    , isPublic = False
    , name = "Player 1"
    , newGameid = ""
    , publicGames = emptyPublicGames
    , chat = ""
    , chatInput = ""
    , chatScroll = -1000
    }

send : Model -> ServerInterface msg -> Message -> Cmd msg
send model (ServerInterface interface) message =
    let si = ServerInterface { interface | placeOnly = placeOnly model }
    in
        Interface.send si (log "send" message)

initialPlayerName : Int -> Model -> String
initialPlayerName number model =
    if (if number == 1 then model.newIsLocal else model.isLocal) then
        let names = String.split "," model.name
        in
            case LE.getAt (number-1) names of
                Nothing -> "Player " ++ (toString number)
                Just n ->
                    let nam = String.trim n
                    in
                        if nam == "" then
                            "Player " ++ (toString number)
                        else
                            nam
    else
        model.name

init : ( Model, Cmd Msg )
init =
    let getString = Http.send ReceiveServerUrl
                    <| Http.getString "server.txt"
    in
        ( initialModel
        , getString
        )

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.isLocal then
        Sub.none
    else
        Sub.batch
            [ if model.connected then
                  WebSocket.listen
                      (Interface.getServer model.server) WebSocketMessage
              else
                  -- This is for simulating connection loss
                  -- Chat "disconnect" / "connect" to switch off / on.
                  Sub.none
            , Time.every Time.second Tick
            ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model
            , Cmd.none
            )
        SetChatInput text ->
            ( { model | chatInput = text }
            , Cmd.none
            )
        SendChat ->
            let text = String.trim model.chatInput
            in
                if text == "" then
                    ( model
                    , Cmd.none
                    )
                else
                    ( { model
                          | chatInput = ""
                          , connected = if text == "connect" then
                                            True
                                        else if text == "disconnect" then
                                            False
                                        else
                                            model.connected
                      }
                    , send model model.server
                        <| ChatReq { playerid = model.playerid
                                   , text = text
                                   }
                    )
        ChatKeydown keycode ->
            if keycode == 13 then
                update SendChat model
            else
                ( model
                , Cmd.none
                )
        ChatScroll scroll ->
            let s = scroll-1
            in
                if s >= model.chatScroll then
                    ( { model | chatScroll = s }
                    , Task.attempt (\_ -> Noop)
                        <| Scroll.toBottom "chat"
                    )
                else
                    ( model
                    , Cmd.none
                    )                    
        SetPage page ->
            if page == PublicPage then
                switchToPublicPage model
            else
                switchFromPublicPage model page
        RefreshPublicGames ->
            ( model
            , send model model.server GamesReq
            )
        SetPlayers players ->
            ( { model | newPlayers = players }
            , Cmd.none
            )
        SetIsLocal local ->
            ( { model | newIsLocal = local }
            , Cmd.none
            )
        SetIsGlobal public ->
            ( { model | isPublic = public }
            , Cmd.none
            )
        SetName name ->
            ( { model | name = name }
            , Cmd.none
            )
        SetGameid gameid ->
            ( { model | newGameid = gameid }
            , Cmd.none
            )
        SetServerUrl url ->
            ( { model | serverUrl = url }
            , Cmd.none
            )
        SetPlaceOnly placeOnly ->
            ( { model | placeOnly = placeOnly }
            , Cmd.none
            )
        SetRestoreState restoreState ->
            ( { model | restoreState = restoreState }
            , Cmd.none
            )
        NewGame ->
            newGame model Nothing
        RestoreGame ->
            case decodeRestoreState (log "restoreState" model.restoreState) of
                Err _ ->
                    addChat model "Malformed restore string."
                Ok state ->
                    let players = List.length state.players
                    in
                        if players /= 2 && players /= 4 then
                            addChat model "Malformed restore string."
                        else
                            newGame { model | newPlayers = players }
                                <| Just state
        JoinPublicGame gameid ->
            -- Uses the existing server, which was queried for the games list.
            joinGame { model
                         | localServer = Nothing
                         , newIsLocal = False
                         , serverUrl = Interface.getServer model.server
                     }
                gameid
        JoinGame ->
            joinGame model model.newGameid
        ResignGame ->
            ( { model
                  | phase = if model.players == 2 then
                                ResignedPhase
                            else
                                model.phase
                  , resignedPlayers = (if model.isLocal then
                                           1
                                       else
                                           model.playerNumber
                                      )
                                      :: model.resignedPlayers
                  , newGameid = ""
              }
            , send model model.server
                <| ResignReq { playerid = if model.isLocal then
                                              "1"
                                          else
                                              model.playerid
                             }
            )
        SetInput player value ->
            ( { model | inputs = Array.set (player-1) value model.inputs }
            , Cmd.none
            )
        Focus player ->
            ( { model | lastFocus = player }
            , Cmd.none
            )
        SetInputColor color ->
            ( { model | inputColor = color }
            , Cmd.none
            )
        Undo ->
            undoMove model
        Place ->
            case getPlacements model of
                Just (move :: _) ->
                    ( { model | placement = Just move }
                    , send model model.server
                        <| PlaceReq { playerid = if model.isLocal then
                                                     "1"
                                                 else
                                                     model.playerid
                                    , placement = move
                                    }
                    )
                _ ->
                    ( model, Cmd.none )
        NodeClick nodeName ->
            if isResigned model 0 then
                ( model, Cmd.none )
            else
                case model.selectedPile of
                    Nothing ->
                        if placeOnly model then
                            placeOnlyClick model nodeName
                        else if (model.phase /= PlacementPhase) ||
                            (String.all Char.isDigit nodeName)
                        then
                            ( model, Cmd.none )
                        else
                            let c = colorLetter model.inputColor
                                input = c ++ nodeName
                            in
                                ( { model
                                      | inputs
                                        = Array.set
                                        (model.lastFocus-1) input model.inputs
                                      , lastFocus
                                        = if model.isLocal then
                                              (model.lastFocus % model.players) + 1
                                          else
                                              model.lastFocus
                                  }
                                , Cmd.none
                                )
                    Just pile ->
                        maybeMakeMove nodeName pile model
        PileClick pile ->
            if isResigned model 0 then
                ( model, Cmd.none )
            else if (placeOnly model) && (model.selectedPile == Nothing) then
                placeOnlyClick model pile.nodeName
            else if (model.phase /= ResolutionPhase) ||
                ((not model.isLocal) &&
                     (model.resolver /= model.playerNumber))
            then
                ( model, Cmd.none )
            else
                case model.selectedPile of
                    Nothing ->
                        case pile.resolutions of
                            [] ->
                                ( model, Cmd.none )
                            _ ->
                                ( { model | selectedPile = Just pile }
                                , Cmd.none
                                )
                    Just p ->
                        if p == pile then
                            ( { model | selectedPile = Nothing }
                            , Cmd.none
                            )
                        else
                            maybeMakeMove pile.nodeName p model
        WebSocketMessage string ->  
            case decodeMessage string of
                Err err ->
                    let _ = log "  error" (err, string)
                    in
                        ( model, Cmd.none )
                Ok message ->
                    let m = case message of
                                ErrorRsp _ ->
                                    model
                                _ ->
                                    { model
                                        | responseCount = model.responseCount + 1
                                    }
                    in
                        serverResponse m model.server message
        ServerResponse server message ->
            serverResponse model server message
        ReceiveServerUrl response ->
            case log "server.txt response" response of
                Err err ->
                    ( model, Cmd.none )
                Ok url ->
                    ( { model | serverUrl = String.trim url }
                    , Cmd.none
                    )
        Tick time ->
            let m = { model | time = time }
            in
                case m.nextResponseCountTime of
                    Nothing ->
                        (m, Cmd.none)
                    Just rct ->
                        maybeSendResponseCountReq m time rct

nextResponseCountReqPeriod : Time
nextResponseCountReqPeriod =
    15 * Time.second

maybeSendResponseCountReq : Model -> Time -> Time -> (Model, Cmd Msg)
maybeSendResponseCountReq model time nextResponseCountTime =
    if time < nextResponseCountTime then
        (model, Cmd.none)
    else
        ( { model |
                nextResponseCountTime = Just <| time + nextResponseCountReqPeriod
          }
        , send model model.server
            <| ResponseCountReq { playerid = model.playerid
                                , number = model.responseCount
                                }
        )

newGame : Model -> Maybe RestoreState -> (Model, Cmd Msg)
newGame model restoreState =
    let isLocal = model.newIsLocal
        isPublic = model.isPublic
        server = if isLocal then
                     initialModel.server
                 else
                     makeServer model.serverUrl Noop
        players = model.newPlayers
        (board, displayList, resolver) =
            processRestoreState initialModel restoreState
    in                             
        ( { initialModel
              | board = board
              , displayList = displayList
              , resolver = resolver
              , restoreState = model.restoreState
              , players = players
              , newPlayers = players
              , name = model.name
              , isLocal = isLocal
              , newIsLocal = isLocal
              , isPublic = isPublic
              , server = server
              , serverUrl = model.serverUrl
              , gameid = ""
              , newGameid = ""
              , time = model.time
          }
        , send model server
            <| NewReq { players = players
                      , name = initialPlayerName 1 model
                      , isPublic = isPublic
                      , restoreState = restoreState
                      }
        )

joinGame : Model -> String -> (Model, Cmd Msg)
joinGame model gameid =
    let isLocal = model.newIsLocal
        server = if isLocal then
                     initialModel.server
                 else
                     makeServer model.serverUrl Noop
    in
        if gameid == model.gameid then
            ( model
            , Cmd.none
            )
        else
            ( { initialModel
                  | restoreState = model.restoreState
                  , name = model.name
                  , isLocal = isLocal
                  , newIsLocal = isLocal
                  , server = server
                  , gameid = gameid
                  , newGameid = gameid
                  , serverUrl = model.serverUrl
                  , time = model.time
              }
            , send model server
                <| JoinReq { gameid = gameid
                           , name = model.name
                           }
            )

placeOnlyClick : Model -> String -> (Model, Cmd Msg)
placeOnlyClick model nodeName =
    if not <| isPlaying model then
        (model, Cmd.none)
    else
        case Board.getNode nodeName model.board of
            Nothing ->
                (model, Cmd.none)
            Just node ->
                let count = node.whiteStones + node.blackStones
                    players = model.players
                in
                    if count >= players then
                        (model, Cmd.none)
                    else
                        ( model
                        , send model model.server
                            <| PlaceReq { playerid = "1"
                                        , placement =
                                            Placement model.inputColor nodeName
                                        }
                        )

switchToPublicPage : Model -> (Model, Cmd Msg)
switchToPublicPage model =
    let m2 = if model.page == PublicPage || (not model.isLocal) then
                 { model | page = PublicPage }
             else
                 { model
                     | page = PublicPage
                     , localServer = Just model.server
                     , isLocal = False
                     , server = makeServer model.serverUrl Noop
                 }
    in
        ( m2
        , send m2 m2.server GamesReq
        )

switchFromPublicPage : Model -> Page -> (Model, Cmd Msg)
switchFromPublicPage model page =
    let m2 = { model | page = page }
    in
        ( if model.page == PublicPage then
              case model.localServer of
                  Nothing ->
                      m2
                  Just server ->
                      { m2
                          | server = server
                          , isLocal = True
                          , localServer = Nothing
                      }
          else
              m2
        , Cmd.none
        )

addChat : Model -> String -> (Model, Cmd Msg)
addChat model message =
    let chat = if model.chat == "" then
                   model.chat
               else
                   model.chat ++ "\n"
    in
        ( { model | chat = chat ++ message }
        , Task.attempt (\res ->
                            case res of
                                Ok scroll -> ChatScroll scroll
                                Err _ -> Noop
                       )
            <| Scroll.y "chat"
        )

placedRsp : Model -> String -> List Move -> Model
placedRsp model gameid placements =
    let board = List.foldr makeMove model.board placements
        his = case model.history of
                  [] ->
                      -- won't happen
                      let turn = newTurn 1 1
                      in
                          [{ turn | placements = placements }]
                  turn :: tail ->
                      let tl = if turn.placements /= [] then
                                   turn :: tail
                               else
                                   tail
                      in
                          { turn | placements = placements }
                          :: tl
        displayList = computeDisplayList board model.renderInfo
        (resolver, phase, history) = resolution Nothing displayList model his
    in
        { model
            | history = history
            , board = board
            , resolver = resolver
            , phase = phase
            , lastFocus = if model.isLocal then 1 else model.lastFocus
            , displayList = displayList
            , inputs = initialInputs
            , placement = Nothing
        }

serverResponse : Model -> ServerInterface Msg -> Message -> (Model, Cmd Msg)
serverResponse mod server message =
    let model = { mod | server = server }
    in
        case log "message" message of
            ChatRsp { text, number } ->
                let name = getPlayerName number "Player " model
                in
                    addChat model <| name ++ ": " ++ text
            NewRsp { gameid, playerid, name, restoreState } ->
                ( let (board, displayList, resolver) =
                          processRestoreState model restoreState
                  in
                      { model
                          | gameid = gameid
                          , board = board
                          , displayList = displayList
                          , resolver = resolver
                          , newGameid = if model.isLocal then
                                            model.newGameid
                                        else
                                            gameid
                          , phase = JoinPhase
                          , playerid = playerid
                          , playerNames = [(1, name)]
                      }
                , if model.isLocal then
                      send model server
                          <| JoinReq { gameid = gameid
                                     , name = initialPlayerName 2 model
                                     }
                  else
                      Cmd.none
                )
            JoinRsp { gameid, players, name, playerid, number, restoreState } ->
                let done = number >= players
                    cmd = if (not done) && model.isLocal then
                              send model server
                                  <| JoinReq { gameid = gameid
                                             , name = initialPlayerName
                                                      (number+1) model
                                             }
                          else
                              Cmd.none
                    playerNames = (number, name) :: model.playerNames
                    (pid, playerNumber, lastFocus) =
                        case playerid of
                            Nothing ->
                                ( model.playerid
                                , model.playerNumber
                                , model.lastFocus)
                            Just pid ->
                                ( pid
                                , number
                                , if model.isLocal then
                                      model.lastFocus
                                  else
                                      number
                                )
                    (board, displayList, resolver) =
                        processRestoreState model restoreState
                    model2 = { model
                                 | board = board
                                 , displayList = displayList
                                 , resolver = resolver
                                 , players = players
                                 , newPlayers = players
                                 , playerNames = playerNames
                                 , playerid = pid
                                 , playerNumber = playerNumber
                                 , lastFocus = lastFocus
                             }
                in
                ( if done then
                      { model2
                          | phase = if displayList.unresolvedPiles == [] then
                                        PlacementPhase
                                    else
                                        ResolutionPhase
                          , turn = 1
                          , nextResponseCountTime =
                            if model.isLocal then
                                Nothing
                            else
                                Just <| model.time + nextResponseCountReqPeriod
                      }
                  else
                      { model2 | phase = JoinPhase }
                , cmd
                )
            PlaceRsp { gameid, number } ->
                if not model.isLocal then
                    ( { model
                          | inputs = Array.set (number-1) "xxxx" model.inputs
                      }
                    , Cmd.none
                    )
                else
                    if number < model.players then
                        case getPlacements model of
                            Nothing ->
                                (model, Cmd.none)
                            Just placements ->
                                case LE.getAt number placements of
                                    Nothing ->
                                        (model, Cmd.none)
                                    Just placement ->
                                        ( model
                                        , send model model.server
                                            <| PlaceReq { playerid =
                                                              toString (number+1)
                                                        , placement = placement
                                                        }
                                        )
                    else
                        (model, Cmd.none)
            PlacedRsp { gameid, placements } ->
                ( placedRsp model gameid placements
                , Cmd.none
                )
            ResolveRsp record ->
                let move = record.resolution
                    board = makeMove move model.board
                    displayList = computeDisplayList board model.renderInfo
                    (resolver, phase, history) =
                        resolution (Just move) displayList model model.history
                in
                    ( { model
                          | history = history
                          , board = board
                          , displayList = displayList
                          , selectedPile = Nothing
                          , resolver = resolver
                          , phase = phase
                      }
                    , Cmd.none
                    )
            ResponseCountRsp { gameid, number, restoreState } ->
                if gameid /= model.gameid then
                    ( model, Cmd.none )
                else
                    responseCountRsp model gameid number restoreState
            ResignRsp { gameid, number, placements } ->
                if gameid /= model.gameid then
                    ( model, Cmd.none )
                else
                    let m = { model
                                | resignedPlayers =
                                    adjoin number model.resignedPlayers
                            }
                        name = getPlayerName number "Player " model
                        (resolver, history) =
                            if (number == m.resolver) then
                                let res = nextResolver m
                                in
                                    ( res
                                    , 
                                        case m.history of
                                            turn :: tail ->
                                                { turn | resolver = res } :: tail
                                            his ->
                                                his
                                    )
                            else
                                (m.resolver, m.history)
                        m2 = { m | resolver = resolver
                             , history = history
                             }
                        m3 = case placements of
                                 Nothing ->
                                     m2
                                 Just plcmnts ->
                                     placedRsp m2 gameid plcmnts
                    in
                        if model.isLocal then
                            ( m3
                            , send m3 m3.server
                                <| ResignReq { playerid = toString (number+1) }
                            )
                        else
                            addChat m3 <| name ++ " resigned."
            GameOverRsp { gameid, reason } ->
                if gameid /= model.gameid then
                    ( model, Cmd.none )
                else
                    let model2 =
                            case reason of
                                UnresolvableReason moves ->
                                    makeGameOverReasonMoves
                                        model server gameid moves
                                HomeCircleFullReason _ moves ->
                                    makeGameOverReasonMoves
                                        model server gameid moves
                                _ ->
                                    model
                    in
                        ( { model2
                              | resignedPlayers =
                                case reason of
                                    ResignationReason p ->
                                        adjoin p model.resignedPlayers
                                    _ ->
                                        model.resignedPlayers
                              , restoreState = createRestoreState model
                              , phase = GameOverPhase reason
                              , newGameid = ""
                              , nextResponseCountTime = Nothing
                          }
                        , Cmd.none
                        )
            GamesRsp games ->
                ( { model | publicGames = games }
                , Cmd.none
                )
            UndoRsp record ->
                case record.message of
                    PlacedRsp { placements } ->
                        let board = List.foldr Board.undoMove model.board placements
                            his = case model.history of
                                      [] ->
                                          []
                                      turn :: tail ->
                                          if turn.placements == [] then
                                              case tail of
                                                  [] ->
                                                      []
                                                  trn :: tl ->
                                                      { trn | placements = [] }
                                                          :: tl
                                          else
                                              { turn | placements = [] } :: tail
                            dl = computeDisplayList board model.renderInfo
                        in
                            ( { model
                                  | board = board
                                  , displayList = dl
                                  , history = his
                                  , phase = PlacementPhase
                              }
                            , Cmd.none
                            )
                    ResolveRsp { resolution } ->
                        let board = Board.undoMove resolution model.board
                            default = (\_ ->
                                           ([], model.turn, model.resolver)
                                      )
                            (his, turn, resolver) =
                                case model.history of
                                      [] ->
                                          default () --can't happen
                                      head :: tail ->
                                          case head.resolutions of
                                              [] ->
                                                  case tail of
                                                      [] ->
                                                          default () --can't happen
                                                      hd :: tl ->
                                                          ( { hd | resolutions =
                                                                  butLast
                                                                  hd.resolutions
                                                            } :: tl
                                                          , hd.number
                                                          , hd.resolver
                                                          )
                                              resolutions ->
                                                  ( { head
                                                        | resolutions =
                                                          butLast resolutions
                                                    } :: tail
                                                  , model.turn
                                                  , model.resolver
                                                  )
                            dl = computeDisplayList board model.renderInfo
                        in
                            ( { model
                                  | board = board
                                  , displayList = dl
                                  , history = his
                                  , phase = ResolutionPhase
                                  , turn = turn
                                  , resolver = resolver
                              }
                            , Cmd.none
                            )
                    _ ->
                        (model, Cmd.none)
            _ ->
                ( model
                , Cmd.none
                )

responseCountRsp : Model -> String -> Int ->  RestoreState -> (Model, Cmd Msg)
responseCountRsp model gameid responseCount restoreState =
    let (board, displayList, resolver) =
            processRestoreState model (Just restoreState)
        phase = model.phase
        newPhase = if phase /= PlacementPhase && phase /= ResolutionPhase then
                       phase
                   else
                       if displayList.unresolvedPiles == [] then
                           PlacementPhase
                       else
                           ResolutionPhase
        m = { model
                | board = board
                , displayList = displayList
                , resolver = resolver
                , responseCount = responseCount + 1
                , phase = newPhase
            }
    in
        addChat m
            <| "Restored state. Missed "
                ++ (toString <| responseCount - model.responseCount + 1)
                ++ " server responses."

processRestoreState : Model -> Maybe RestoreState -> (Board, DisplayList, Int)
processRestoreState model restoreState =
    case restoreState of
        Nothing ->
            (model.board, model.displayList, model.resolver)
        Just rs ->
            let board = Board.encodedStringToBoard rs.board
            in
                ( board
                , computeDisplayList board model.renderInfo
                , rs.resolver
                )

makeGameOverReasonMoves : Model -> ServerInterface Msg -> String -> List Move -> Model
makeGameOverReasonMoves model server gameid moves =
    let response = case moves of
                       Placement _ _ :: _ ->
                           PlacedRsp { gameid = gameid
                                     , placements = moves
                                     }
                       resolution :: _ ->
                           ResolveRsp { gameid = gameid
                                      , resolution = resolution
                                      }
                       _ ->
                           ChatRsp { gameid = gameid
                                   , text = "Empty moves in game over response."
                                   , number = 0
                                   }
        (model2, _) = serverResponse model server response
    in
        model2

undoMove : Model -> (Model, Cmd Msg)
undoMove model =
    let history = model.history
        board = model.board
    in
        case history of
            [] ->
                (model, Cmd.none)
            { number, resolver, placements, resolutions } :: tail ->
                case List.reverse resolutions of
                    [] ->
                        case placements of
                            [] ->
                                case tail of
                                    [] ->
                                        (model, Cmd.none)
                                    { number, resolver } :: _ ->
                                        let (_, cmd) = undoMove
                                                       { model
                                                           | turn = number
                                                           , resolver = resolver
                                                           , history = tail
                                                       }
                                        in
                                            (model, cmd)
                            _ ->
                                ( model
                                , send model model.server
                                    <| UndoReq { playerid = model.playerid
                                               , message =
                                                   PlacedRsp
                                                   { gameid = model.gameid
                                                   , placements = placements
                                                   }
                                               }
                                )
                    resolution :: _ ->
                        ( model
                        , send model model.server
                            <| UndoReq { playerid = model.playerid
                                       , message =
                                           ResolveRsp { gameid = model.gameid
                                                      , resolution = resolution
                                                      }
                                       }
                        )

nextResolver : Model -> Int
nextResolver model =
    let resigned = model.resignedPlayers
        initialResolver = model.resolver
        players = model.players
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

resolution : Maybe Move -> DisplayList -> Model -> History -> (Int, ServerPhase, History)
resolution maybeMove displayList model history =
    let resolved = displayList.unresolvedPiles == []
        resolver = if resolved then
                       nextResolver model
                   else
                       model.resolver
        phase = if resolved then PlacementPhase else ResolutionPhase
        his = case history of
                  [] ->
                      []    --can't happen
                  turn :: tail ->
                      let t2 = case maybeMove of
                                   Nothing ->
                                       turn
                                   Just move ->
                                       { turn
                                           | resolutions
                                             = List.append
                                             turn.resolutions
                                             [ move ]
                                       }
                          his2 = t2 :: tail
                      in
                          if resolved then
                              (newTurn (turn.number + 1) resolver) :: his2
                          else
                              his2
    in
        (resolver, phase, his)

maybeMakeMove : String -> StonePile -> Model -> ( Model, Cmd Msg )
maybeMakeMove nodeName pile model =
    case findResolution nodeName pile of
        Nothing ->
            ( model, Cmd.none )
        Just move ->
            ( model
            , send model model.server
                <| ResolveReq { playerid = model.playerid
                              , resolution = move
                              }
            )

br : Html Msg
br =
    Html.br [] []

b : List (Html msg) -> Html msg
b body =
    Html.b [] body

center : List (Attribute msg) -> List (Html msg) -> Html msg
center =
    node "center"

getPlacements : Model -> Maybe (List Move)
getPlacements model =
    let inputs = model.inputs
        board = model.board
        loop = (\idx res ->
                    if idx < 0 then
                        Just res
                    else
                        case Array.get idx inputs of
                            Nothing ->
                                Nothing
                            Just string ->
                                case isLegalPlacement string board of
                                    Err _ ->
                                        Nothing
                                    Ok move ->
                                        if model.isLocal then
                                            loop (idx-1) <| move :: res
                                        else
                                            Just [move]
               )
    in
        loop (if model.isLocal then
                   model.players - 1
              else
                  model.playerNumber - 1
             )
            []

canPlace : Model -> Bool
canPlace model =
    case getPlacements model of
        Nothing ->
            False
        Just _ ->
            True

startLine : Model -> Html Msg
startLine model =
    span []
        [ text "Click 'New Game' or 'Join Game' below to begin. "
        , button [ onClick <| SetPage HelpPage ] [ text "Help" ]
        ]

joinLine : Model -> Html Msg
joinLine model =
    span []
        [ text "Waiting for players to join. "
        , button [ disabled True ] [ text "Wait" ]
        ]

placeOnlyLine : Model -> Html Msg
placeOnlyLine model =
    span []
        [ text "Click anywhere to place stones."
        , button [ disabled True
                 ]
              [ text "Place Only" ]
        ]

placementLine : Model -> Html Msg
placementLine model =
    span []
        [ text
              <| (getPlayerName model.resolver "Player " model) ++
                  " will resolve. "
        , button [ onClick Place
                 , disabled <| (isResigned model 0) || (not <| canPlace model)
                 ]
              [ text "Place" ]
        ]

resolutionLine : Model -> Html Msg
resolutionLine model =
    span []
        [ text <|
              if model.isLocal then
                  "Player " ++ (toString model.resolver) ++ " please resolve. "
              else
                  if model.resolver == model.playerNumber then
                      "Please resolve now. "
                  else
                      (getPlayerName model.resolver "Player " model) ++
                          " is resolving. "
        , button [ disabled True ] [ text "Resolve" ]
        ]

resignedLine : Model -> Html Msg
resignedLine model =
    span []
        [ text "You resigned. Click 'New Game' or 'Join Game' for another game. "
        , button [ disabled True ] [ text "Resigned" ]
        ]

gameOverReasonText : Model -> GameOverReason -> String
gameOverReasonText model reason =
    let pname = (\player ->
                     getPlayerName player "Player " model
                )
    in
        case reason of
            ResignationReason player ->
                (pname player) ++ " resigned."
            UnresolvableReason _ ->
                "Unresolvable."
            HomeCircleFullReason player _ ->
                let name = pname player
                    poss = if name == "YOU" then "YOUR" else name ++ "'s"
                in
                    poss ++ " home circle is full."
            TimeoutReason ->
                "The server timed out."
            UnknownReason text ->
                text

unresignedPlayers : Model -> List Int
unresignedPlayers model =
    let all = Set.fromList <| List.range 1 model.players
        resigned = Set.fromList model.resignedPlayers
    in
        Set.toList <| Set.diff all resigned        

winner : Model -> (Int, String)
winner model =
    let scores = List.map (\player ->
                               (player, Board.count model.players player model.board)
                          )
                          <| unresignedPlayers model
    in
        case List.sortWith (\(p1, (a1, b1)) (p2, (a2, b2)) ->
                             case compare a2 a1 of
                                 EQ ->
                                     case compare b2 b1 of
                                         EQ ->
                                             compare p2 p1
                                         x ->
                                             x
                                 x ->
                                     x
                           )
                           scores
        of
            (player, _) :: _ ->
                (player, getPlayerName player "Player " model)
            _ ->
                (0, "Nobody")

gameOverLine : Model -> GameOverReason -> Html Msg
gameOverLine model reason =
    span []
        [ text <| "Game over. " ++ (gameOverReasonText model reason) ++ " "
        , let (number, name) = winner model
          in
              b [ if number == model.playerNumber then
                      text "You win!"
                  else
                      span []
                      [ text <| name
                      , text " wins!"
                      ]
                ]
        , text " "
        , button [ disabled True ] [ text "Game Over" ]
        ]

playButton : Html Msg
playButton =
    p []
        [ button [ onClick <| SetPage GamePage
                 , style [("font-size", "150%")]
                 ]
              [ text "Play" ]
        ]

iframe : String -> Html Msg
iframe url =
    Html.iframe [ style [ ("width", "40em")
                        , ("height", "40em")
                        ]
                , src url
                ]
        []

renderIframePage : Model -> String -> Html Msg
renderIframePage model url =
    div []
        [ playButton
        , iframe url
        , playButton
        ]

renderRulesPage : Model -> Html Msg
renderRulesPage model =
    renderIframePage model "docs/rules.html"

renderHelpPage : Model -> Html Msg
renderHelpPage model =
    renderIframePage model "docs/help.html"

isPlaying : Model -> Bool
isPlaying model =
    (List.member model.phase [JoinPhase, PlacementPhase, ResolutionPhase]) &&
        (not <| List.member model.playerNumber model.resignedPlayers)

onKeydown : (Int -> msg) -> Attribute msg
onKeydown tagger =
  on "keydown" (Json.map tagger keyCode)

renderGamePage : Model -> Html Msg
renderGamePage model =
    let nostart = isPlaying model
    in
        div []
            [ inputItems model
            , p []
                [ if (isPlaying model) && (placeOnly model) then
                      placeOnlyLine model
                  else
                      case model.phase of
                          StartPhase -> startLine model
                          JoinPhase -> joinLine model
                          PlacementPhase -> placementLine model
                          ResolutionPhase -> resolutionLine model
                          ResignedPhase -> resignedLine model
                          GameOverPhase reason -> gameOverLine model reason
                ]
            , p []
                [ b [ text "Placement Click Color: " ]
                , radio "color" "white " (model.inputColor == White) False
                    <| SetInputColor White
                , radio "players" "black" (model.inputColor == Black) False
                    <| SetInputColor Black
                ]
            , p []
                [ let renderInfo = model.renderInfo
                      ri = { renderInfo
                               | players = Just model.players
                               , playerNames = model.playerNames
                               , playerNumber =
                                 Just
                                 <| if model.isLocal then
                                        model.resolver
                                    else
                                        model.playerNumber
                               , resolver = Just model.resolver
                               , placement = model.placement
                           }
                  in
                      Board.render
                      model.selectedPile model.displayList ri
                ]
            , if model.newIsLocal then
                  text ""
              else
                  chatParagraph model .chat
            , p [] [ b [ text "Players: " ]
                   , radio "players" "2 " (model.newPlayers == 2) nostart
                       <| SetPlayers 2
                   , radio "players" "4 " (model.newPlayers == 4) nostart
                       <| SetPlayers 4
                   , if model.newIsLocal then
                         text ""
                     else
                         span []
                             [ input [ type_ "checkbox"
                                     , onCheck SetIsGlobal
                                     , checked model.isPublic
                                     , disabled nostart
                                     ]
                                   []
                             , text "public "
                             ]
                   , button [ onClick <| if nostart then ResignGame else NewGame
                            ]
                       [ text <| if nostart then "Resign Game" else "New Game" ]
                   , br
                   , radio "local" "local " (model.newIsLocal) nostart
                       <| SetIsLocal True
                   , radio "local" "remote" (not model.newIsLocal) nostart
                       <| SetIsLocal False
                   , b [ text <| if model.newIsLocal then
                                     " Names: "
                                 else
                                     " Name: "
                       ]
                   , input [ type_ "text"
                           , onInput SetName
                           , disabled nostart
                           , size 30
                           , value model.name
                           ]
                         []
                   , if model.newIsLocal then
                         span []
                             [ br
                             , input [ type_ "checkbox"
                                     , onCheck SetPlaceOnly
                                     , checked model.placeOnly
                                     ]
                                 []
                             , text " Place Only"
                             ]
                     else
                         span []
                             [ br
                             , b [text " URL: " ]
                             , input [ type_ "text"
                                     , onInput <| SetServerUrl
                                     , disabled (model.newIsLocal || nostart)
                                     , size 50
                                     , value model.serverUrl
                                     ]
                                     []
                             , br
                             , b [ text "Game ID: " ]
                             , if nostart then
                                   span [ style [ ("font-size","140%")
                                                , ("font-weight","bold")
                                                ]
                                        ]
                                       [ text model.gameid ]
                               else
                                   span []
                                       [ input [ type_ "text"
                                               , onInput SetGameid
                                               , disabled nostart
                                               , size 18
                                               , value model.newGameid
                                               ]
                                             []
                                       , text " "
                                       , button [ onClick JoinGame
                                                , disabled nostart
                                                ]
                                             [ text "Join Game" ]
                                       ]
                             ]
                   , br
                   , input [ type_ "text"
                           , onInput SetRestoreState
                           , disabled nostart
                           , size 60
                           , value <| encodedRestoreState model nostart
                           ]
                         []
                   , text " "
                   , button [ onClick RestoreGame
                            , disabled nostart
                            ]
                       [ text "Restore" ]
                   ]
            ]

chatParagraph : Model -> (Model -> String) -> Html Msg
chatParagraph model accessor =
    p []
        [ textarea [ id "chat"
                   , class "chat"
                   , readonly True
                   ]
              -- TODO: make player names bold.
              [ text <| accessor model ]
        , br
        , b [ text "Chat: " ]
        , input [ type_ "text"
                , onInput SetChatInput
                , onKeydown ChatKeydown
                , size 50
                , value model.chatInput
                ]
              []
        , text " "
        , button [ onClick SendChat ]
            [ text "Send" ]
        ]

renderGames : Model -> List PublicGame -> Bool -> String -> Html Msg
renderGames model games nostart players =
    p []
        [ h3 [] [ text players
                , text "-Player Games"
                ]
        , if games == [] then
              text "No games"
          else
              table [ class "bordered" ]
                  <| (tr []
                          [ th [] [ text "Players" ]
                          , th [] [ text "Join" ]
                          ]
                     ) ::
                  (List.map (renderGameRow nostart) games)
        ]

renderGameRow : Bool -> PublicGame -> Html Msg
renderGameRow nostart game =
    tr []
        [ td []
              <| List.map (\name -> span [] [ text name, br ]) game.playerNames
        , td []
            [ button [ onClick <| JoinPublicGame game.gameid
                     , disabled nostart
                     ]
                  [ text "Join" ]
            ]
        ]

renderPublicPage : Model -> Html Msg
renderPublicPage model =
    let games = model.publicGames
        nostart = isPlaying model
    in
        div []
            [ playButton
            , p []
                [ b [ text "Name: " ]
                , input [ type_ "text"
                        , onInput SetName
                        , disabled nostart
                        , size 30
                        , value model.name
                        ]
                      []
                , text " "
                , button [ onClick RefreshPublicGames ] [ text "Refresh" ]
                ]
            , let twoPlayer = games.twoPlayer
                  fourPlayer = games.fourPlayer
              in
                  if twoPlayer == [] && fourPlayer == [] then
                      p [] [ b [ text "There are no public games." ] ]
                  else
                      div []
                          [ if twoPlayer == [] then
                                text ""
                            else
                                renderGames model twoPlayer nostart "Two"
                          , if fourPlayer == [] then
                                text ""
                            else
                                renderGames model fourPlayer nostart "Four"
                          ]
            , playButton
            ]

pages : List (Page, String)
pages =
    [ ( HelpPage, "Help" )
    , ( PublicPage, "Public")
    , ( RulesPage, "Rules" )
    ]

pageLink : Page -> (Page, String) -> Html Msg
pageLink currentPage (page, label) =
    span []
        [ text " "
        , if currentPage == page then
              span [ class "pagename" ] [ text label ]
          else
              a [ href "#", onClick <| SetPage page ]
                  [ text label ]
        ]

pageLinks : Page -> Html Msg
pageLinks currentPage =
    span []
        <| List.map (pageLink currentPage) pages

style_ = node "style"

view : Model -> Html Msg
view model =
    center []
        [ style_ [ type_ "text/css"]
              [ text "@import \"style.css\"" ]
        , h2 [] [ text "Spokes" ]
        , p [] [ pageLinks model.page ]
        , case model.page of
              GamePage ->
                  renderGamePage model
              PublicPage ->
                  renderPublicPage model
              RulesPage ->
                  renderRulesPage model
              HelpPage ->
                  renderHelpPage model
        , p [] [ pageLinks model.page
               , br
               , a [ href "https://gibgoygames.com/"
                   , target "_blank"
                   ]
                   [ text "Gib Goy Games" ]
               , text " "
               , a [ href "https://github.com/billstclair/spokes"
                   , target "_blank"
                   ]
                   [ text "GitHub" ]
               ]
        , p [] [ text "Invented by Chris St. Clair"
               , br
               , text "Coded by Bill St. Clair"
               , br
               , text "Made with "
               , a [ href "http://elm-lang.org/"
                   , target "_blank"
                   ]
                   [ text "Elm" ]
               ]
        , if model.page == GamePage then
              historyDiv model
          else
              text ""
        ]

historyDiv : Model -> Html Msg
historyDiv model =
    div []
        [ p []
              [ button [ onClick Undo
                       , disabled
                             <| (not model.isLocal) &&
                                 ( model.phase /= ResolutionPhase ||
                                       model.resolver /= model.playerNumber ||
                                       (case model.history of
                                            { resolutions } :: _ ->
                                                resolutions == []
                                            _ ->
                                                True
                                       )
                                 )
                       ]
                    [ text "Undo" ]
              ]
        , table [ class "bordered" ]
            <| (tr []
                    [ th [] [ text "Turn #" ]
                    , th [] [ text "Resolver" ]
                    , th [] [ text "Details" ]
                    ]
               ) :: (List.map turnRow model.history)
        ]

turnRow : Turn -> Html Msg
turnRow turn =
    tr []
        [ td [] [ text <| toString turn.number ]
        , td [] [ text <| toString turn.resolver ]
        , td [ style [("text-align", "left")] ]
            <| if turn.placements == [] then
                   [ text "" ]
               else
                   [ span []
                         [ b [ text "Place: " ]
                         , text <| placementsHistoryText turn.placements
                         ]
                   , case turn.resolutions of
                         [] ->
                             text ""
                         resolutions ->
                             span []
                                 (br
                                 :: ( b [ text "Resolve: " ] )
                                 :: ( List.concatMap renderResolution resolutions )
                                 )
                   ]
        ]
    
placementsHistoryText : List Move -> String
placementsHistoryText moves =
    let strings = List.filter (\x -> x /= "")
                  <| List.map placementText moves
    in
        String.concat <| List.intersperse ", " strings

renderResolution : Move -> List (Html Msg)
renderResolution move =
    case move of
        Resolution moved from to ->
            [ br
            , text <|
                (movedStoneString moved) ++ ": " ++ from ++ " to " ++ to
            ]
        _ ->
            [ text "" ]


isEven : Int -> Bool
isEven int =
    int % 2 == 0

examplePlaceString : Int -> String
examplePlaceString player =
    let bw = if isEven player then "w" else "b"
        circle = case player of
                     1 -> "b"
                     2 -> "d"
                     3 -> "d"
                     _ -> "c"
        spoke = case player of
                    1 -> 1
                    2 -> 12
                    3 -> 9
                    _ -> 4
    in
        bw ++ circle ++ (toString spoke)

getPlayerName : Int -> String -> Model -> String
getPlayerName player prefix model =
    if (not model.isLocal) && (player == model.playerNumber) then
        "YOU"
    else
        case LE.find (\(n, _) -> n == player) model.playerNames of
            Nothing ->
                prefix ++ (toString player)
            Just (_, name) ->
                name

getPlayerNames : Model -> List String
getPlayerNames model =
    List.map Tuple.second <| List.sortBy (\(n,_) -> n) model.playerNames

isResigned : Model -> Int -> Bool
isResigned model number =
    List.member (if number == 0 then model.playerNumber else number)
        model.resignedPlayers

inputItem : Int -> Model -> Html Msg
inputItem player model =
    if isResigned model player then
        if player == model.playerNumber then
            span []
                [ span [ class "resigned" ]
                      [ text "YOU" ]
                , text " resigned. "
                ]
           else
               span []
                   [ span [ class "resigned" ]
                         [ text <| getPlayerName player "Player" model ]
                   , text " resigned. "
                   ]
    else
        span []
            [ b [ text <| getPlayerName player "" model 
                , text ": "
                ]
            , input [ type_ "text"
                    , onInput <| SetInput player
                    , disabled
                          <| (model.phase /= PlacementPhase) ||
                              ((not model.isLocal) && (player /= model.playerNumber))
                    , placeholder
                          <| if player == model.lastFocus &&
                              model.phase == PlacementPhase
                             then
                                 examplePlaceString player
                             else
                                 ""
                , size 5
                , value (Maybe.withDefault "" <| Array.get (player-1) model.inputs)
                , onFocus <| Focus player
                ]
              []
        , text " "
        , text (toString <| Board.count model.players player model.board)
        , text " "
        ]

inputItems : Model -> Html Msg
inputItems model =
    div []
        <| List.map (\player -> inputItem player model)
        <| List.range 1 model.players

radio : String -> String -> Bool -> Bool -> msg -> Html msg
radio name_ value isChecked isDisabled msg =
    span [ onClick msg ]
        [ input
            [ type_ "radio"
            , name name_
            , checked isChecked
            , disabled isDisabled
            ]
            []
        , text value
        ]

createRestoreState : Model -> String
createRestoreState model =
    encodeRestoreState { board = Board.boardToEncodedString model.board
                       , players = getPlayerNames model
                       , resolver = model.resolver
                       }

encodedRestoreState : Model -> Bool -> String
encodedRestoreState model nostart =
    if nostart && model.phase /= JoinPhase then
        createRestoreState model
    else
        model.restoreState
