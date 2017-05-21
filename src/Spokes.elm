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

import Spokes.Types as Types exposing ( Msg(..), Board, RenderInfo, Move
                                      , DisplayList, emptyDisplayList
                                      , StonePile, Color(..)
                                      )
import Spokes.Board as Board exposing ( render, isLegalPlacement, makeMove
                                      , computeDisplayList, findResolution
                                      )

import Html exposing ( Html, Attribute
                     , div, text, span, p, h2, h3, a, node
                     , input, table, tr, td, button
                     )
import Html.Attributes exposing ( value, size, maxlength, href, src, title
                                , alt, style, selected, type_, name, checked
                                , placeholder, disabled
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
    = Placement
    | Resolution

type alias Model =
    { board : Board
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
    }

initialInputs : Array String
initialInputs = Array.repeat 4 ""

initialModel : Model
initialModel =
    { board = Board.initialBoard
    , renderInfo = Board.renderInfo 600
    , displayList = emptyDisplayList
    , players = 2
    , newPlayers = 2
    , turn = 1
    , phase = Placement
    , lastFocus = 1
    , inputColor = White
    , inputs = initialInputs
    , resolver = 1
    , selectedPile = Nothing
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
        Place ->
            case getPlacements model of
                Nothing ->
                    ( model, Cmd.none )
                Just moves ->
                    let board = List.foldr
                                (\move b -> makeMove move b)
                                model.board
                                moves
                        displayList = computeDisplayList
                                      board model.renderInfo
                        (resolver, phase) = resolution displayList model
                    in
                        ( { model
                              | board = board
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
                    if (model.phase == Resolution) ||
                        (String.all Char.isDigit nodeName) then
                        ( model, Cmd.none )
                    else
                        let c = case model.inputColor of
                                    White -> "W"
                                    Black -> "B"
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

resolution : DisplayList -> Model -> (Int, Phase)
resolution displayList model =
    let resolved = displayList.unresolvedPiles == []
        resolver = if resolved then
                       (model.resolver % model.players) + 1
                   else
                       model.resolver
        phase = if resolved then Placement else Resolution
    in
        (resolver, phase)

maybeMakeMove : String -> StonePile -> Model -> ( Model, Cmd Msg )
maybeMakeMove nodeName pile model =
    case findResolution nodeName pile of
        Nothing ->
            ( model, Cmd.none )
        Just move ->
            let board = makeMove move model.board
                displayList = computeDisplayList board model.renderInfo
                (resolver, phase) = resolution displayList model
            in
                ( { model
                      | board = board
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
                        Just <| List.reverse res
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
    text <| "Player " ++ (toString model.resolver) ++ " please resolve."

view : Model -> Html Msg
view model =
    center []
        [ h2 [] [ text "Spokes" ]
        , inputItems model
        , p []
            [ case model.phase of
                  Placement -> placementLine model
                  Resolution -> resolutionLine model
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
        , p [] [ a [ href "rules/" ] [ text "Rules" ]
               , br
               , a [ href "https://github.com/billstclair/spokes" ]
                   [ text "GitHub" ]
               ]
        , p [] [ text "Invented by Chris St. Clair"
               , br
               , text "Coded by Bill St. Clair"
               ]
        ]

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
                , disabled <| model.phase == Resolution
                , placeholder
                      <| if player == model.lastFocus &&
                          model.phase == Placement
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
