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
                     )
import Html.Attributes exposing ( value, size, maxlength, href, src, title
                                , alt, style, selected )
import Html.Events exposing ( onClick, onInput, on, keyCode )

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
    }

initialModel : Model
initialModel =
    { board = Board.initialBoard
    , renderInfo = Board.renderInfo 600
    , players = 2
    , turn = 1
    , phase = Placement
    }

init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )

type Msg
    = Nothing

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )

br : Html Msg
br =
    Html.br [] []

center : List (Attribute msg) -> List (Html msg) -> Html msg
center =
    node "center"

view : Model -> Html Msg
view model =
    center []
        [ h2 [] [ text "Spokes" ]
        , p [] [ text "Invented by Christopher St. Clair"
               , br
               , text "Coded by Bill St. Clair"
               ]
        , p []
            [ Board.render model.board model.renderInfo ]
        , p [] [ a [ href "rules/" ] [ text "Rules" ]
               , br
               , a [ href "https://github.com/billstclair/spokes" ]
                   [ text "GitHub" ]
               ]
        ]
