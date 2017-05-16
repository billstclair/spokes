----------------------------------------------------------------------
--
-- Spokes.elm
-- Christopher St. Clair's Spokes board game, in Elm.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Spokes exposing (..)

import Spokes.Types as Types exposing ( Board, RenderInfo )
import Spokes.Board as Board exposing ( render )

import Html exposing ( Html, Attribute
                     , div, text, span, p, h2, h3, a, node
                     , input, table, tr, td
                     )
import Html.Attributes exposing ( value, size, maxlength, href, src, title
                                , alt, style, selected, type_, name, checked )
import Html.Events exposing ( onClick, onInput, on, keyCode )
import Array exposing ( Array )

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
    , players : Int
    , turn : Int
    , phase : Phase
    , inputs : Array String
    }

initialModel : Model
initialModel =
    { board = Board.initialBoard
    , renderInfo = Board.renderInfo 600
    , players = 2
    , turn = 1
    , phase = Placement
    , inputs = Array.empty
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

type Msg
    = SetPlayers Int
    | SetInput Int String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlayers players ->
            ( { model | players = players }
            , Cmd.none
            )
        SetInput player value ->
            ( { model | inputs = Array.set player value model.inputs }
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

view : Model -> Html Msg
view model =
    center []
        [ h2 [] [ text "Spokes" ]
        , inputItems model
        , p []
            [ Board.render model.board model.renderInfo ]
        , p [] [ b [ text "Players:" ]
               , radio "players" "2 " (model.players == 2) <| SetPlayers 2
               , radio "players" "4" (model.players == 4) <| SetPlayers 4
               ]
        , p [] [ a [ href "rules/" ] [ text "Rules" ]
               , br
               , a [ href "https://github.com/billstclair/spokes" ]
                   [ text "GitHub" ]
               ]
        , p [] [ text "Invented by Christopher St. Clair"
               , br
               , text "Coded by Bill St. Clair"
               ]
        ]

inputItem : Int -> Model -> Html Msg
inputItem player model =
    span []
        [ b [ text <| (toString player) ++ ": " ]
        , input [ type_ "text"
                , onInput <| SetInput player
                , size 5
                ]
              []
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
