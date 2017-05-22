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

module Spokes.Types exposing ( Page(..), Msg(..), Board, Node
                             , Point, Sizes, RenderInfo
                             , Color(..), MovedStone(..), NodeClassification(..)
                             , Move(..), History
                             , StonePile, DisplayList
                             , zeroPoint, emptyStonePile, emptyDisplayList
                             , get, set
                             )

import Dict exposing ( Dict )

type Page
    = GamePage
    | RulesPage
    | HelpPage

type Msg
    = SetPlayers Int
    | NewGame
    | SetInput Int String
    | Place
    | NodeClick String
    | PileClick StonePile
    | Focus Int
    | SetInputColor Color
    | SetPage Page

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

type NodeClassification
    = Empty
    | BlackOnly
    | WhiteOnly
    | WhiteWhite
    | BlackBlack
    | Blocked

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

type alias XPlist a =
    List (String, a)

get : String -> XPlist a -> Maybe a
get key plist =
    case plist of
        [] ->
            Nothing
        (k, v) :: rest ->
            if key == k then
                Just v
            else
                get key rest

set : String -> a -> XPlist a -> XPlist a
set key value plist =
    (key, value) :: (List.filter (\(k,_) -> k /= key) plist)
