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
                                      , Turn, History,
                                      , ServerInterface, Message(..)
                                      , movedStoneString
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
import Debug exposing ( log )

main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\x -> Sub.none)
        }

type Phase
    = PlacementPhase
    | ResolutionPhase

type alias Model =
    { page : Page
    , history : History
    , board : Board
    , renderInfo : RenderInfo
    , displayList : DisplayList
    , players : Int
    , newPlayers : Int
    , turn : Int
    , phase : Phase
    , lastFocus : Int
    , inputColor : Color
    , inputs : Array String
    , resolver : Int
    , selectedPile : Maybe StonePile
    , server : ServerInterface Msg
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
    , turn = 1
    , phase = PlacementPhase
    , lastFocus = 1
    , inputColor = White
    , inputs = initialInputs
    , resolver = 1
    , selectedPile = Nothing
    , server = makeProxyServer ServerResponse
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

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
                Nothing ->
                    ( model, Cmd.none )
                Just moves ->
                    let board = List.foldr makeMove model.board moves
                        his = case model.history of
                                  [] ->
                                      -- won't happen
                                      let turn = newTurn 1 1
                                      in
                                          [{ turn | placements = moves}]
                                  turn :: tail ->
                                      { turn | placements = moves }
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
        NodeClick nodeName ->
            case model.selectedPile of
                Nothing ->
                    if (model.phase == ResolutionPhase) ||
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
            ( { model | server = server }
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
                                        undoMove { model
                                                     | turn = number
                                                     , resolver = resolver
                                                     , history = tail
                                                 }
                            _ ->
                                let b = List.foldr Board.undoMove board placements
                                    his = { number = number
                                          , resolver = resolver
                                          , placements = []
                                          , resolutions = []
                                          }
                                          :: tail
                                    dl = computeDisplayList b model.renderInfo
                                in
                                    ( { model
                                          | board = b
                                          , displayList = dl
                                          , history = his
                                          , phase = PlacementPhase
                                      }
                                    , Cmd.none
                                    )
                    res :: restail ->
                        let b = Board.undoMove res board
                            his = { number = number
                                  , resolver = resolver
                                  , placements = placements
                                  , resolutions = List.reverse restail
                                  }
                                  :: tail
                            dl = computeDisplayList b model.renderInfo
                        in
                            ( { model
                                  | board = b
                                  , displayList = dl
                                  , history = his
                                  , phase = ResolutionPhase
                              }
                            , Cmd.none
                            )

resolution : Maybe Move -> DisplayList -> Model -> History -> (Int, Phase, History)
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
            let board = makeMove move model.board
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
                , disabled <| model.phase == ResolutionPhase
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
