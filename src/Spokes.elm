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

import Board exposing ( Board, initialBoard )

import Html exposing ( Html, Attribute
                     , div, text, span, p, h2, h3
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
    { board : Int
    , players : Int
    , turn : Int
    , phase : Phase
    }

initialModel : Model
initialModel =
    { board = 0
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

view : Model -> Html Msg
view model =
    div []
        [ text "Hello World." ]
