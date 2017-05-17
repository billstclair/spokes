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

module Spokes.Board exposing ( initialBoard, renderInfo, render
                             , parseNodeName, count
                             , isLegalMove, isLegalPlacement, makeMove
                             )

import Spokes.Types as Types exposing ( Board, Node
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
    let (circle, spoke) = Result.withDefault ("", 0)
                          <| parseNodeName name
    in
        { name = name
        , circle = circle
        , spoke = spoke
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
        , stoneRadius = radius // 16 --bRadius/4
        }

computeStoneLocations : Int -> Int -> Int -> Float -> (Point, Point)
computeStoneLocations stoneRadius x y theta =
    let sr = toFloat stoneRadius
        dx = round((cos theta) * sr)
        dy = round((sin theta) * sr)
    in
        ( { x = x + dx
          , y = y + dy
          }
        , { x = x - dx
          , y = y - dy
          }
        )

circlePointLocations : String -> Int -> Int -> Int -> Int -> Int -> Float -> List (String, Point, Point, (Point, Point))
circlePointLocations circle center radius stoneRadius count textRDelta textThetaDelta =
    let is = List.range 0 (count-1)
        loc = (\i ->
                   let theta = 2.0 * pi * (toFloat i) / (toFloat count)
                       r = toFloat radius
                       textTheta = theta + textThetaDelta
                       textR = toFloat <| radius - ({-if circle == "" then
                                                        2 * textRDelta // 3
                                                    else-}
                                                        textRDelta
                                                   )
                       x = (sin theta) * r
                       y = (cos theta) * r
                       textX = if radius == 0 then
                                   center + (textRDelta // 2)
                               else
                                   center + (round <| (sin textTheta) * textR)
                       textY = if radius == 0 then
                                   center - (textRDelta // 2)
                               else
                                   center - (round <| (cos textTheta) * textR)
                       ix = center + (round x)
                       iy = center - (round y)
                   in
                       ( { x = ix
                         , y = iy
                         }
                       , { x = textX
                         , y = textY
                         }
                       , computeStoneLocations stoneRadius ix iy theta
                       )
              )
    in
        List.map (\i ->
                      let (location, textLoc, stoneLocs) = loc i
                          label = circle ++ (toString (i+1))
                      in
                          (label, location, textLoc, stoneLocs)
                 )
                 is

renderInfo : Int -> RenderInfo
renderInfo diameter =
    let sizes = sizesFromDiameter diameter
        radius = toFloat sizes.radius
        sr = sizes.stoneRadius
        dr = 28
        drf = toFloat dr
        dt = 8.0 * pi / 360.0
        cpl = (\circle r count ->
                   circlePointLocations
                   circle sizes.center r sr count dr
                   (dt * radius / (if r == 0 then 1 else toFloat r))
              )
        locs = List.concat
               [ cpl "A" 0 1
               , cpl "B" sizes.bRadius 4
               , cpl "C" sizes.cRadius 8
               , cpl "D" sizes.dRadius 16
               , cpl "" sizes.radius 16
               ]
        locations = List.map (\(c, l, _, _) -> (c, l)) locs
        textLocations = List.map (\(c, _, l, _) -> (c, l)) locs
        stoneLocations = List.map (\(c, _, _, l) -> (c, l)) locs
        
    in
        { sizes = sizes
        , locations = Dict.fromList locations
        , textLocations = Dict.fromList textLocations
        , stoneLocations = Dict.fromList stoneLocations
        }

circle : String -> String -> Svg msg
circle center radius =
    Svg.circle [ cx center, cy center, r radius ]
        []

render : Board -> RenderInfo -> Html msg
render board info =
    let sizes = info.sizes
        indent = 20
        is = toString indent
        w = toString (sizes.diameter + indent + indent)
        d = toString sizes.diameter
        c = toString sizes.center
        r = toString sizes.radius
        rb = toString sizes.bRadius
        rc = toString sizes.cRadius
        rd = toString sizes.dRadius
    in
        svg [ width w, height w ]
            [ g [ transform <| "translate("++is++","++is++")"
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
                      , renderStones board info
                      ]
            ]

renderPoints : Board -> RenderInfo -> List (Svg msg)
renderPoints board info =
    let sizes = info.sizes
        sr = toString sizes.stoneRadius
        draw = (\p ->
                    let x = toString p.x
                        y = toString p.y
                    in
                        Svg.circle [cx x, cy y, r "5", fillOpacity "1"] []
               )
        drawStone = (\p color ->
                         let x = toString p.x
                             y = toString p.y
                         in
                             Svg.circle [ cx x, cy y, r sr
                                        , fillOpacity "1"
                                        , fill color
                                        ]
                                 []
                    )
        drawStones = (\(p1, p2) -> [ drawStone p1 "white"
                                   , drawStone p2 "black"
                                   ])
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
            [ Dict.toList info.locations
              |> List.map Tuple.second
              |> List.map draw
            , Dict.toList info.textLocations
              |> List.map drawText
            , if debugStoneLocations then
                  Dict.toList info.stoneLocations
                    |> List.map Tuple.second
                    |> List.concatMap drawStones
              else
                  []
            ]

partitionStones : Int -> Int -> List (List String)
partitionStones black white =
    let total = black + white
    in
        if total == 0 then
            []
        else if total == 1 then
            [ if black > 0 then
                  [ "black" ]
              else
                  [ "white" ]
            ]
        else if total == 2 then
            [ if black == 2 then
                  [ "black", "black" ]
              else if white == 2 then
                  [ "white", "white" ]
              else
                  [ "black", "white" ]
            ]
        else if total == 3 then
            if black > 0 && white > 0 then
                [ [ "black", "white" ]
                , [ if black == 2 then
                        "black"
                    else
                        "white"
                  ]
                ]
            else if black == 3 then
                [["black","black"],["black"]]
            else
                [["white","white"],["white"]]
        else if black > 0 && white > 0 then
            [ [ "black", "white" ]
            , if black == 1 then
                  [ "white", "white" ]
              else if black == 2 then
                  [ "black", "white" ]
              else
                  [ "black", "black" ]
            ]
        else if black == 0 then
            [["white","white"],["white","white"]]
        else
            [["black","black"],["black","black"]]

renderStones : Board -> RenderInfo -> List (Svg msg)
renderStones board info =
    let sizes = info.sizes
        sr = toString sizes.stoneRadius
        locs = info.locations
        slocs = info.stoneLocations
        delta = 10
        drawStone = (\x y color outline ->
                         Svg.circle [ cx (toString x)
                                    , cy (toString y)
                                    , r sr
                                    , fillOpacity "1"
                                    , fill color
                                    , stroke
                                          <| Maybe.withDefault "darkgray" outline
                                    ]
                         []
                    )
        isBlock = (\stones ->
                       stones == ["white","black"] || stones == ["black","white"]
                  )
        drawPile = (\p stones twoPiles otherBlock ->
                        let outline = if (not otherBlock) && (isBlock stones) then
                                          Nothing
                                      else if not twoPiles then
                                          if List.length stones > 1 then
                                              Just "red"
                                          else
                                              Nothing
                                      else
                                              Just "red"
                        in
                            case stones of
                                [] ->
                                    []
                                [ stone ] ->
                                    [ drawStone p.x p.y stone outline ]
                                s1 :: s2 :: _ ->
                                    [ drawStone p.x (p.y + delta) s1 outline
                                    , drawStone p.x p.y s2 outline
                                    ]
                   )
        drawNode = (\node ->
                          let name = node.name
                              p = Maybe.withDefault zeroPoint
                                    <| Dict.get name locs
                              (sp1, sp2) = Maybe.withDefault (zeroPoint, zeroPoint)
                                           <| Dict.get name slocs
                              ws = node.whiteStones
                              bs = node.blackStones
                          in
                              case partitionStones bs ws of
                                  [] ->
                                      []
                                  [ stones ] ->
                                      drawPile p stones False False
                                  s1 :: s2 :: _ ->
                                      List.concat [ drawPile sp1 s1 True False
                                                  , drawPile sp2 s2 True (isBlock s1)
                                                  ]
                   )
    in
        Dict.toList board
            |> List.map Tuple.second
            |> List.concatMap drawNode

debugStoneLocations : Bool
debugStoneLocations =
    False

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

getNode : String -> Board -> Maybe Node
getNode name board =
    Dict.get name board

setNode : String -> Node -> Board -> Board
setNode name node board =
    Dict.insert name node board

playMove : Move -> Board -> Board
playMove move board =
    case move of
        Placement color name ->
            case getNode name board of
                Nothing ->
                    board
                Just node ->
                    let n = case color of
                                White ->
                                    { node | whiteStones = node.whiteStones + 1 }
                                Black ->
                                    { node | blackStones = node.blackStones + 1 }
                    in
                        Dict.insert name n board
        Resolution color from to ->
            case getNode from board of
                Nothing ->
                    board
                Just fromNode ->
                    case getNode to board of
                        Nothing ->
                            board
                        Just toNode ->
                            let (fn, tn) =
                                    case color of
                                        White ->
                                            ( { fromNode
                                                  | whiteStones
                                                    = fromNode.whiteStones - 1
                                              }
                                            , { toNode
                                                  | whiteStones
                                                    = toNode.whiteStones + 1
                                              }
                                            )
                                        Black ->
                                            ( { fromNode
                                                  | blackStones
                                                    = fromNode.blackStones - 1
                                              }
                                            , { toNode
                                                  | blackStones
                                                    = toNode.blackStones + 1
                                              }
                                            )
                            in
                                if from == to then
                                    board
                                else
                                    board
                                        |> Dict.insert from fn
                                        |> Dict.insert to tn

nodeNeedsResolution : Node -> Bool
nodeNeedsResolution node =
    not (node.whiteStones<=1 && node.blackStones<=1)

isNodeBlocked : Node -> Bool
isNodeBlocked node =
    node.whiteStones==1 && node.blackStones==1

nodesNeedingResolution : Board -> List Node
nodesNeedingResolution board =
    let f = (\name node nodes ->
                 if nodeNeedsResolution node then
                     node :: nodes
                 else
                     nodes
            )
    in
        Dict.foldl f [] board

blockedNode : Node
blockedNode =
    { name = "blocked"
    , circle = ""
    , spoke = 0         
    , connections = []
    , whiteStones = 1
    , blackStones = 1
    }

getNodeWithDefault : String -> Node -> Board -> Node
getNodeWithDefault name default board =
    case getNode name board of
        Just node ->
            node
        Nothing ->
            default

-- TODO
-- Doesn't follow rules yet. Allows any move.
possibleResolutions : Node -> Board -> List Move
possibleResolutions node board =
    if not <| nodeNeedsResolution node then
        []
    else
        let white = node.whiteStones
            black = node.blackStones
            from = node.name
        in
            List.concatMap (\to ->
                                if isNodeBlocked
                                     <| getNodeWithDefault to blockedNode board
                                then
                                    []
                                else
                                    List.concat
                                      [ if white >= 1 && (not <| white<=black) then
                                            [ Resolution White from to ]
                                        else
                                            []
                                      , if black >= 1 && (not <| black<=white) then
                                            [ Resolution Black from to ]
                                        else
                                            []
                                      ]
                           )
                           node.connections

parseNodeName : String -> Result String (String, Int)
parseNodeName nodeName =
    let circle = String.toUpper <| String.left 1 nodeName
        (c, i) = if circle=="A" || circle=="B" || circle=="C" || circle=="D" then
                     (circle, String.dropLeft 1 nodeName)
                 else
                     ("", nodeName)
    in
        case String.toInt i of
            Ok spoke ->
                Ok (c, spoke)
            Err msg ->
                Err msg            

twoPlayerSpokes : Dict Int (Dict String (List Int))
twoPlayerSpokes =
    Dict.fromList
        [ (1, Dict.fromList
               [ ("B", List.range 1 2)
               , ("C", List.range 1 4)
               , ("D", List.range 1 8)
               , ("", List.range 1 8)
               ]
          )
        , (2, Dict.fromList
               [ ("B", List.range 3 4)
               , ("C", List.range 5 8)
               , ("D", List.range 9 16)
               , ("", List.range 9 16)
               ]
          )
        ]

fourPlayerSpokes : Dict Int (Dict String (List Int))
fourPlayerSpokes =
    Dict.fromList
        [ (1, Dict.fromList
               [ ("B", List.range 1 1)
               , ("C", List.range 1 2)
               , ("D", List.range 1 4)
               , ("", List.range 1 4)
               ]
          )
        , (2, Dict.fromList
               [ ("B", List.range 2 2)
               , ("C", List.range 3 4)
               , ("D", List.range 5 8)
               , ("", List.range 5 8)
               ]
          )
        , (3, Dict.fromList
               [ ("B", List.range 3 3)
               , ("C", List.range 5 6)
               , ("D", List.range 9 12)
               , ("", List.range 9 12)
               ]
          )
        , (4, Dict.fromList
               [ ("B", List.range 4 4)
               , ("C", List.range 7 8)
               , ("D", List.range 13 16)
               , ("", List.range 13 16)
               ]
          )
        ]

count : Int -> Int -> Board -> (Int, Int)
count players player board =
    let dict = if players == 2 then
                   twoPlayerSpokes
               else
                   fourPlayerSpokes
    in
        case Dict.get player dict of
            Nothing ->
                (0, 0)
            Just spokesDict ->
                Dict.foldl (\name node (outer, total) ->
                                case Dict.get node.circle spokesDict of
                                    Nothing ->
                                        (outer, total)
                                    Just spokes ->
                                        let cnt = node.whiteStones +
                                                  node.blackStones
                                        in
                                            if List.member node.spoke spokes then
                                                if node.circle == "" then
                                                    (outer+cnt, total+cnt)
                                                else
                                                    (outer, total+cnt)
                                            else
                                                (outer, total)
                           )
                    (0, 0)
                    board

parsePlacementMove : String -> Result String Move
parsePlacementMove string =
    let color = case String.toUpper <| String.left 1 string of
                    "W" -> Just White
                    "B" -> Just Black
                    _ -> Nothing
    in
        case color of
            Nothing ->
                Err <| "Bad color in: '" ++ string ++ "'"
            Just c ->
                case parseNodeName <| String.dropLeft 1 string of
                    Err msg ->
                        Err msg
                    Ok (circle, spoke) ->
                        if circle == "" then
                            Err "Can't place in home circle."
                        else
                            Ok <| Placement c <| circle ++ (toString spoke)

isLegalMove : Move -> Board -> Bool
isLegalMove move board =
    case move of
        Placement color nodeName ->
            case getNode nodeName board of
                Nothing ->
                    False
                Just node ->
                    node.whiteStones==0 && node.blackStones==0
        Resolution color from to ->
            -- TODO
            True

isLegalPlacement : String -> Board -> Result String Move
isLegalPlacement string board =
    case parsePlacementMove string of
        Err msg ->
            Err <| "Can't parse placement: " ++ msg
        Ok move ->
            if isLegalMove move board then
                Ok move
            else
                Err <| "Not legal move: " ++ string

makeMove : Move -> Board -> Board
makeMove move board =
    case move of
        Placement color nodeName ->
            case getNode nodeName board of
                Nothing ->
                    board
                Just node ->
                    let n = case color of
                                White -> { node
                                             | whiteStones = node.whiteStones + 1
                                         }
                                Black -> { node
                                             | blackStones = node.blackStones + 1
                                         }
                    in
                        setNode nodeName n board
        Resolution color from to ->
            -- TODO
            board
