----------------------------------------------------------------------
--
-- Board.elm
-- Spokes game board.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Spokes.Types exposing ( Board, Node
                             , Point, Sizes, RenderInfo
                             , Color(..), Move(..), History
                             , zeroPoint
                             )

import Dict exposing ( Dict )

type alias Board =
    Dict String Node

type alias Node =
    { name : String
    , circle : String
    , spoke : Int
    , connections : List String
    , whiteStones : Int
    , blackStones : Int
    }

type alias Point =
    { x : Int
    , y : Int
    }

zeroPoint =
    { x = 0, y = 0 }

type alias Sizes =
    { diameter : Int
    , center : Int
    , bRadius : Int
    , cRadius : Int
    , dRadius : Int
    , radius : Int
    }

type alias RenderInfo =
    { sizes : Sizes
    , locations : Dict String Point
    , textLocations : Dict String Point
    }

type Color = Black | White

type Move
    = Placement Color String
    | Resolution Color String String

type alias History =
    List Move
