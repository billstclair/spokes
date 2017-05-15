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

module Board exposing ( Board, initialBoard
                      )

import Dict exposing ( Dict )
import Svg exposing (Svg, svg, line, circle, g)
import Svg.Attributes exposing ( x, y, width, height
                               , x1, y1, x2, y2
                               , fill, stroke, fontSize, transform
                               )

type alias Board =
    Dict String Node

type alias Node =
    { name : String
    , connections : List String
    , whiteStones : Int
    , blackStones : Int
    }

node : String -> List String -> Node
node name connections =
    { name = name
    , connections = connections
    , whiteStones = 0
    , blackStones = 0
    }

initialNodes : List Node
initialNodes =
    List.map (\(x, y) -> node x y)
        [ ("A1",["B1","B2", "B3", "B4"])

        , ("B1",["A1","B4","B2","C1"])
        , ("B2",["A1","B1","B3","C3"])
        , ("B3",["A1","B2","B4","C5"])
        , ("B4",["A1","B3","B1","C7"])

        , ("C1",["B1","C1","C3","D1"])
        , ("C2",["C1","C3","D3"])
        , ("C3",["B2","C2","C4","D5"])
        , ("C4",["C3","C5","D7"])
        , ("C5",["B3","C4","C6","D9"])
        , ("C6",["C5","C7","D11"])
        , ("C7",["B4","C6","C8","D13"])
        , ("C8",["C7","C1","D15"])

        , ("D1",["C1","D16","D2","1"])
        , ("D2",["D1","D3","2"])
        , ("D3",["C2","D2","D3","3"])
        , ("D4",["D3","D5","4"])
        , ("D5",["C3","D4","D3","5"])
        , ("D6",["D5","D7","6"])
        , ("D7",["C4","D6","D8","7"])
        , ("D8",["D7","D9","8"])
        , ("D9",["C5","D8","D19","9"])
        , ("D10",["D9","D11","10"])
        , ("D11",["C6","D10","D12","11"])
        , ("D12",["D11","D13","12"])
        , ("D13",["C7","D12","D14","13"])
        , ("D14",["D13","D15","14"])
        , ("D15",["C8","D14","D16","15"])
        , ("D16",["D15","D1","16"])

        , ("1",["D1","16","2"])
        , ("2",["D2","1","3"])
        , ("3",["D3","2","3"])
        , ("4",["D4","3","5"])
        , ("5",["D5","4","3"])
        , ("6",["D6","5","7"])
        , ("7",["D7","6","8"])
        , ("8",["D8","7","9"])
        , ("9",["D9","8","19"])
        , ("10",["D10","9","11"])
        , ("11",["D11","10","12"])
        , ("12",["D12","11","13"])
        , ("13",["D13","12","14"])
        , ("14",["D14","13","15"])
        , ("15",["D15","14","16"])
        , ("16",["D16","15","1"])
        ]

initialBoard : Board
initialBoard =
    Dict.fromList <| List.map (\n -> (n.name, n)) initialNodes
