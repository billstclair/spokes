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
                                      , Turn, History
                                      , ServerPhase(..), ServerInterface, Message(..)
                                      , movedStoneString, butLast
                                      )
import Spokes.Board as Board exposing ( render, isLegalPlacement, makeMove
                                      , computeDisplayList, findResolution
                                      , placementText, colorLetter
                                      )
import Spokes.Server.Interface exposing ( makeProxyServer, send )

import Html exposing ( Html, Attribute
                     , div, text, span, p, h2, h3, a, node
                     , input, table, tr, th, td, button
                     )
import Html.Attributes exposing ( value, size, maxlength, href, src, title
                                , alt, style, selected, type_, name, checked
                                , placeholder, disabled, target
                                , width, height, class
                                )
import Html.Events exposing ( onClick, onInput, onFocus )
import Array exposing ( Array )
import Char
import List.Extra as LE
import Debug exposing ( log )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\x -> Sub.none)
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
    , turn : Int
    , phase : ServerPhase
    , lastFocus : Int
    , inputColor : Color
    , inputs : Array String
    , resolver : Int
    , selectedPile : Maybe StonePile
    , server : ServerInterface Msg
    , gameid : String
    }

initialInputs : Array String
initialInputs = Array.repeat 4 ""

newTurn : Int -> Int -> Turn
newTurn number resolver =
    { number = number
    , resolver = resolver
    , placements = []
    , resolutions = []
    }

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
    , turn = 1
    , phase = JoinPhase
    , lastFocus = 1
    , inputColor = White
    , inputs = initialInputs
    , resolver = 1
    , selectedPile = Nothing
    , server = makeProxyServer ServerResponse
    , gameid = ""
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel
    , send initialModel.server
        <| NewReq { players = initialModel.players
                  , name = "Bill"
                  }
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            ( { model | page = page }
            , Cmd.none
            )
        SetPlayers players ->
            ( { model | newPlayers = players }
            , Cmd.none
            )
        NewGame ->
            ( { initialModel
                  | players = model.newPlayers
                  , newPlayers = model.newPlayers
              }
            , Cmd.none
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
                    ( model
                    , send model.server
                        <| PlaceReq { gameid = model.gameid
                                    , placement = move
                                    , number = 1
                                    }
                    )
                _ ->
                    ( model, Cmd.none )
        NodeClick nodeName ->
            case model.selectedPile of
                Nothing ->
                    if (model.phase /= PlacementPhase) ||
                        (String.all Char.isDigit nodeName) then
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
                                      = (model.lastFocus % model.players) + 1
                              }
                            , Cmd.none
                            )
                Just pile ->
                    maybeMakeMove nodeName pile model
        PileClick pile ->
            case model.selectedPile of
                Nothing ->
                    case pile.resolutions of
                        Nothing ->
                            ( model, Cmd.none )
                        Just _ ->
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
        ServerResponse server message ->
            serverResponse model server message

serverResponse : Model -> ServerInterface Msg -> Message -> (Model, Cmd Msg)
serverResponse mod server message =
    let model = { mod | server = server }
    in
        case log "message" message of
            NewRsp { gameid, name } ->
                ( { model
                      | gameid = gameid
                      , playerNames = [(1, name)]
                  }
                , send server
                    <| JoinReq { gameid = gameid, name = "bill" }
                )
            JoinRsp { gameid, number, name } ->
                let done = number >= model.players
                    cmd = if not done then
                              send server
                                  <| JoinReq { gameid = gameid, name = "bill" }
                          else
                              Cmd.none
                    playerNames = (number, name) :: model.playerNames
                in
                ( if done then
                      { model
                          | playerNames = playerNames
                          , phase = PlacementPhase
                          , turn = 1
                          , resolver = 1
                      }
                  else
                      { model
                          | playerNames = playerNames
                      }
                , cmd
                )
            PlaceRsp { gameid, number } ->
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
                                    , send model.server
                                        <| PlaceReq { gameid = gameid
                                                    , placement = placement
                                                    , number = number + 1
                                                    }
                                    )
                else
                    (model, Cmd.none)
            PlacedRsp { gameid, placements } ->
                let board = List.foldr makeMove model.board placements
                    his = case model.history of
                              [] ->
                                  -- won't happen
                                  let turn = newTurn 1 1
                                  in
                                      [{ turn | placements = placements }]
                              turn :: tail ->
                                  { turn | placements = placements }
                                  :: tail
                    displayList = computeDisplayList
                                  board model.renderInfo
                    (resolver, phase, history)
                        = resolution Nothing displayList model his
                in
                    ( { model
                          | history = history
                          , board = board
                          , resolver = resolver
                          , phase = phase
                          , lastFocus = 1
                          , displayList = displayList
                          , inputs = initialInputs
                      }
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
                                , send model.server
                                    <| UndoReq { gameid = model.gameid
                                               , message =
                                                   PlacedRsp
                                                   { gameid = model.gameid
                                                   , placements = placements
                                                   }
                                               }
                                )
                    resolution :: _ ->
                        ( model
                        , send model.server
                            <| UndoReq { gameid = model.gameid
                                       , message =
                                           ResolveRsp { gameid = model.gameid
                                                      , resolution = resolution
                                                      }
                                       }
                        )

resolution : Maybe Move -> DisplayList -> Model -> History -> (Int, ServerPhase, History)
resolution maybeMove displayList model history =
    let resolved = displayList.unresolvedPiles == []
        resolver = if resolved then
                       (model.resolver % model.players) + 1
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
            , send model.server
                <| ResolveReq { gameid = model.gameid
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
                                        loop (idx-1) <| move :: res
               )
    in
        loop (model.players - 1) []

canPlace : Model -> Bool
canPlace model =
    case getPlacements model of
        Nothing ->
            False
        Just _ ->
            True

joinLine : Model -> Html Msg
joinLine model =
    span []
        [ text "Waiting for players to join. "
        , button [ disabled True ] [ text "Place" ]
        ]

placementLine : Model -> Html Msg
placementLine model =
    span []
        [ text <| "Player " ++ (toString model.resolver) ++ " will resolve. "
        , button [ onClick Place
                 , disabled (not <| canPlace model)
                 ]
              [ text "Place" ]
        ]

resolutionLine : Model -> Html Msg
resolutionLine model =
    span []
        [ text <| "Player " ++ (toString model.resolver) ++ " please resolve. "
        , button [ disabled True ] [ text "Place" ]
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

renderIframePage : String -> String -> Html Msg
renderIframePage title url =
    div []
        [ h3 [] [ text title ]
        , playButton
        , iframe url
        , playButton
        ]

renderRulesPage : Html Msg
renderRulesPage =
    renderIframePage "Rules" "docs/rules.html"

renderHelpPage : Html Msg
renderHelpPage =
    renderIframePage "Help" "docs/help.html"

renderGamePage : Model -> Html Msg
renderGamePage model =
    div []
        [ inputItems model
        , p []
            [ case model.phase of
                  JoinPhase -> joinLine model
                  PlacementPhase -> placementLine model
                  ResolutionPhase -> resolutionLine model
            ]
        , p []
            [ b [ text "Placement Click Color: " ]
            , radio "color" "white " (model.inputColor == White)
                  <| SetInputColor White
            , radio "players" "black" (model.inputColor == Black)
                <| SetInputColor Black
            ]
        , p []
            [ Board.render model.selectedPile model.displayList model.renderInfo ]
        , p [] [ b [ text "Players: " ]
               , radio "players" "2 " (model.newPlayers == 2) <| SetPlayers 2
               , radio "players" "4 " (model.newPlayers == 4) <| SetPlayers 4
               , button [ onClick NewGame ] [ text "New Game" ]
               ]
        ]

pages : List (Page, String)
pages =
    [ ( HelpPage, "Help" )
    , ( RulesPage, "Rules" )
    ]

pageLink : Page -> (Page, String) -> Html Msg
pageLink currentPage (page, label) =
    span []
        [ text " "
        , if currentPage == page then
              text label
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
        , case model.page of
              GamePage ->
                  renderGamePage model
              RulesPage ->
                  renderRulesPage
              HelpPage ->
                  renderHelpPage
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
              [ button [ onClick Undo ]
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

inputItem : Int -> Model -> Html Msg
inputItem player model =
    span []
        [ b [ text <| (toString player) ++ ": " ]
        , input [ type_ "text"
                , onInput <| SetInput player
                , disabled <| model.phase /= PlacementPhase
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

radio : String -> String -> Bool -> msg -> Html msg
radio name_ value isChecked msg =
    span [ onClick msg]
        [ input
            [ type_ "radio"
            , name name_
            , checked isChecked
            ]
            []
        , text value
        ]
