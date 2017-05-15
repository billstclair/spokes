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

module Board exposing ( initialBoard, renderInfo, render
                      )

import Types exposing ( Board, Node
                      , Point, Sizes, RenderInfo
                      , Color(..), Move (..), History
                      , zeroPoint
                      )

import Dict exposing ( Dict )
import Html exposing ( Html )
import Svg exposing (Svg, svg, line, g)
import Svg.Attributes exposing ( x, y, width, height
                               , cx, cy, r
                               , x1, y1, x2, y2
                               , fill, stroke, strokeWidth, fontSize, transform
                               , fillOpacity, textAnchor, dominantBaseline
                               )
import String
import Debug exposing ( log )

node : String -> List String -> Node
node name connections =
    { name = name
    , connections = connections
    , whiteStones = 0
    , blackStones = 0
    }

initialNodes : List Node
initialNodes =
    List.map (\(x, y) -> node x y) nodeConnections

nodeConnections : List (String, List String)
nodeConnections =
    [ ("A1",["B1","B2", "B3", "B4"])
          
    , ("B1",["A1","B4","B2","C1"])
    , ("B2",["A1","B1","B3","C3"])
    , ("B3",["A1","B2","B4","C5"])
    , ("B4",["A1","B3","B1","C7"])
        
    , ("C1",["B1","C8","C2","D1"])
    , ("C2",["C1","C3","D3"])
    , ("C3",["B2","C2","C4","D5"])
    , ("C4",["C3","C5","D7"])
    , ("C5",["B3","C4","C6","D9"])
    , ("C6",["C5","C7","D11"])
    , ("C7",["B4","C6","C8","D13"])
    , ("C8",["C7","C1","D15"])
        
    , ("D1",["C1","D16","D2","1"])
    , ("D2",["D1","D3","2"])
    , ("D3",["C2","D2","D4","3"])
    , ("D4",["D3","D5","4"])
    , ("D5",["C3","D4","D3","5"])
    , ("D6",["D5","D7","6"])
    , ("D7",["C4","D6","D8","7"])
    , ("D8",["D7","D9","8"])
    , ("D9",["C5","D8","D10","9"])
    , ("D10",["D9","D11","10"])
    , ("D11",["C6","D10","D12","11"])
    , ("D12",["D11","D13","12"])
    , ("D13",["C7","D12","D14","13"])
    , ("D14",["D13","D15","14"])
    , ("D15",["C8","D14","D16","15"])
    , ("D16",["D15","D1","16"])
        
    , ("1",["D1","16","2"])
    , ("2",["D2","1","3"])
    , ("3",["D3","2","4"])
    , ("4",["D4","3","5"])
    , ("5",["D5","4","6"])
    , ("6",["D6","5","7"])
    , ("7",["D7","6","8"])
    , ("8",["D8","7","9"])
    , ("9",["D9","8","10"])
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
        
sizesFromDiameter : Int -> Sizes
sizesFromDiameter diameter =
    let radius = diameter // 2
    in
        { diameter = diameter
        , center = radius
        , bRadius = radius // 4
        , cRadius = radius // 2
        , dRadius = 3 * radius // 4
        , radius = radius
        }

circlePointLocations : String -> Int -> Int -> Int -> Int -> Float -> List (String, Point, Point)
circlePointLocations circle center radius count textRDelta textThetaDelta =
    let is = List.range 0 (count-1)
        loc = (\i ->
                   let theta = 2.0 * pi * (toFloat i) / (toFloat count)
                       r = toFloat radius
                       textTheta = theta + textThetaDelta
                       textR = toFloat <| radius - (if circle == "" then
                                                        2 * textRDelta // 3
                                                    else
                                                        textRDelta
                                                   )
                       x = (sin theta) * r
                       y = (cos theta) * r
                       textX = if radius == 0 then
                                   center + textRDelta
                               else
                                   center + (round <| (sin textTheta) * textR)
                       textY = if radius == 0 then
                                   center - (textRDelta // 2)
                               else
                                   center - (round <| (cos textTheta) * textR)
                   in
                       ( { x = center + (round x)
                         , y = center - (round y)
                         }
                       , { x = textX
                         , y = textY
                         }
                       )
              )
    in
        log "cpl" <|
        List.map (\i ->
                      let (location, textLoc) = loc i
                          label = circle ++ (toString (i+1))
                      in
                          (label, location, textLoc)
                 )
                 is

renderInfo : Int -> RenderInfo
renderInfo diameter =
    let sizes = sizesFromDiameter diameter
        radius = toFloat sizes.radius
        dr = 20
        drf = toFloat dr
        dt = 8.0 * pi / 360.0
        cpl = (\circle r count ->
                   circlePointLocations
                   circle sizes.center r count dr
                   (dt * radius / (if r == 0 then 1 else toFloat r))
              )
        locs = List.concat
               [ cpl "A" 0 1
               , cpl "B" sizes.bRadius 4
               , cpl "C" sizes.cRadius 8
               , cpl "D" sizes.dRadius 16
               , cpl "" sizes.radius 16
               ]
        locations = List.map (\(c, l, _) -> (c, l)) locs
        textLocations = List.map (\(c, _, l) -> (c, l)) locs
        
    in
        { sizes = sizes
        , locations = Dict.fromList locations
        , textLocations = Dict.fromList textLocations
        }

circle : String -> String -> Svg msg
circle center radius =
    Svg.circle [ cx center, cy center, r radius ]
        []

render : Board -> RenderInfo -> Html msg
render board info =
    let sizes = info.sizes
        w = toString (sizes.diameter + 18)
        d = toString sizes.diameter
        c = toString sizes.center
        r = toString sizes.radius
        rb = toString sizes.bRadius
        rc = toString sizes.cRadius
        rd = toString sizes.dRadius
    in
        svg [ width w, height w ]
            [ g [ transform <| "translate(9,9)"
                , stroke "black"
                , strokeWidth "2"
                , fillOpacity "0"
                ]
                  <| List.concat
                      [ [ circle c r
                        , circle c rb
                        , circle c rc
                        , circle c rd
                        ]
                      , renderPoints board info
                      , renderLines board info
                      ]
            ]

renderPoints : Board -> RenderInfo -> List (Svg msg)
renderPoints board info =
    let sizes = info.sizes
        locs = info.locations
        textLocs = info.textLocations
        draw = (\p ->
                    let x = toString p.x
                        y = toString p.y
                    in
                        Svg.circle [cx x, cy y, r "5", fillOpacity "1"] []
               )
        drawText = (\(c, p) ->
                     Svg.text_
                         [ x <| toString p.x
                         , y <| toString p.y
                         , textAnchor "middle"
                         , dominantBaseline "central"
                         ]
                         [ Svg.text c ]
                   )
    in
        List.concat
            [ Dict.toList locs
              |> List.map Tuple.second
              |> List.map draw
            , Dict.toList textLocs
              |> List.map drawText
            ]

inBiggerCircle : String -> String -> Bool
inBiggerCircle c1 c2 =
    let p1 = String.left 1 c1
        p2 = String.left 1 c2
    in
        if p1 == p2 then
            False
        else case p1 of
                 "A" ->
                     False
                 "B" ->
                     p2 == "A"
                 "C" ->
                     p2 == "A" || p2 == "B"
                 "D" ->
                     p2 == "A" || p2 == "B" || p2 == "C"
                 _ ->
                     if p2 == "A" || p2 == "B" || p2 == "C" || p2 == "D" then
                         True
                     else
                         False                         

renderLines : Board -> RenderInfo -> List (Svg msg)
renderLines board info =
    let locs = info.locations
        drawLine = (\fx fy p ->
                        let tx = toString p.x
                            ty = toString p.y
                        in
                            Svg.line [ x1 fx, y1 fy, x2 tx, y2 ty ] []
                   )
        nodeLines = (\(node, nodes) ->
                         case Dict.get node locs of
                             Nothing -> []
                             Just {x, y} ->
                                 let fx = toString x
                                     fy = toString y
                                 in
                                     List.filter (\n -> inBiggerCircle n node)
                                                 nodes
                                       |> List.map (\n -> Dict.get n locs)
                                       |> List.filter (\l ->
                                                           case l of
                                                               Nothing -> False
                                                               _ -> True
                                                      )
                                       |> List.map (\x -> Maybe.withDefault
                                                        zeroPoint x
                                                   )
                                       |> List.map (\p -> drawLine fx fy p)
                    )
    in
        List.concat <| List.map nodeLines nodeConnections

