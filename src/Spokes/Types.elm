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
                             , Color(..), MovedStone(..), Move(..), History
                             , StonePile, DisplayList
                             , zeroPoint, emptyStonePile, emptyDisplayList
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
    , stoneRadius : Int
    }

type alias RenderInfo =
    { sizes : Sizes
    , locations : Dict String Point
    , textLocations : Dict String Point
    , stoneLocations  : Dict String (Point, Point)
    }

type Color
    = Black
    | White

type MovedStone
    = MoveBlack
    | MoveWhite
    | MoveBlock

type Move
    = Placement Color String
    | Resolution MovedStone String String

type alias History =
    List Move

type alias StonePile =
    { nodeName : String
    , colors : List String
    , location : Point
    , resolutions : Maybe (List Move)
    }

emptyStonePile : StonePile
emptyStonePile =
    { nodeName = ""
    , colors = []
    , location = zeroPoint
    , resolutions = Nothing
    }

type alias DisplayList =
    { allPiles : List StonePile
    , unresolvedPiles : List StonePile
    }

emptyDisplayList : DisplayList
emptyDisplayList =
    { allPiles = []
    , unresolvedPiles = []
    }
