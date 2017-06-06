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
                             , Move(..), Turn, History
                             , StonePile, DisplayList
                             , Message(..), ServerPhase(..)
                             , GameState, ServerState, ServerInterface(..)
                             , zeroPoint, emptyStonePile, emptyDisplayList
                             , get, set, butLast
                             , movedStoneString, stringToMovedStone
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
    | Undo
    | ServerResponse (ServerInterface Msg) Message

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

movedStoneString : MovedStone -> String
movedStoneString stone =
    case stone of
        MoveBlack -> "Black"
        MoveWhite -> "White"
        MoveBlock -> "Block"

stringToMovedStone : String -> Maybe MovedStone
stringToMovedStone string =
    case string of
        "Black" ->
            Just MoveBlack
        "White" ->
            Just MoveWhite
        "Block" ->
            Just MoveBlock
        _ ->
            Nothing

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

type alias Turn =
    { number : Int
    , resolver : Int
    , placements : List Move
    , resolutions : List Move
    }

type alias History =
    List Turn

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

butLast : List a -> List a
butLast list =
    let loop = (\l res ->
                    case l of
                        [_] ->
                            List.reverse res
                        head :: tail ->
                            loop tail <| head :: res
                        [] ->
                            []
               )
    in
        loop list []
                    

---
--- Backend interface
---

type Message
    = RawMessage String String (List (String, String))
    -- Basic game play
    | NewReq { players : Int, name : String }
    | NewRsp { gameid : String, playerid : String, players : Int, name : String }
    | JoinReq { gameid : String, name : String }
    | JoinRsp { gameid : String, name : String, playerid: Maybe String, number : Int }
    | PlaceReq { playerid : String, placement : Move }
    | PlaceRsp { gameid : String, number : Int }
    | PlacedRsp { gameid : String, placements : List Move }
    | ResolveReq { playerid : String, resolution : Move }
    | ResolveRsp { gameid : String, resolution : Move }
    -- Errors
    | UndoReq { playerid : String, message: Message }
    | UndoRsp { gameid : String, message: Message }
    | ErrorRsp { request : String, id : Int, text : String }
    -- Chat
    | ChatReq { playerid : String, text : String }
    | ChatRsp { gameid : String, text : String, number : Int }

type ServerPhase
    = JoinPhase
    | PlacementPhase
    | ResolutionPhase

type alias PlayerInfo =
    { gameid : String
    , number : Int
    , name : String
    }

type alias ServerState =
    { playerInfoDict : Dict String PlayerInfo --playerid -> PlayerInfo
    , playeridDict : Dict String (List String) -- gameid -> List playerid
    , gameDict : Dict String GameState --gameid -> GameState
    }

type alias GameState =
    { board : Board
    , renderInfo : RenderInfo
    , phase : ServerPhase
    , unresolvedPiles : List StonePile
    , players : Int
    , turn : Int
    , resolver : Int
    , placements : Dict Int Move
    , gameid : String
    , history : History
    }

type ServerInterface msg
    = ServerInterface
      { server : String
      , wrapper : ServerInterface msg -> Message -> msg
      , state : Maybe ServerState
      , sender : ServerInterface msg -> Message -> Cmd msg
      }

