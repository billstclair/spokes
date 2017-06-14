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
                             , parseNodeName, count, getNode
                             , parsePlacementMove, placementText, colorLetter
                             , isLegalMove, isLegalPlacement, makeMove, undoMove
                             , computeDisplayList, findResolution
                             , canResolve, boardToString, makePlacements
                             , isHomeCircleFull
                             )

import Spokes.Types as Types exposing ( Msg(..), Board, Node
                                      , Point, Sizes, RenderInfo
                                      , Color(..), Move(..), MovedStone(..)
                                      , NodeClassification(..)
                                      , History, StonePile, DisplayList
                                      , zeroPoint, emptyStonePile
                                      , get, set
                                      )
import Dict exposing ( Dict )
import Set exposing ( Set )
import Html exposing ( Html )
import Svg exposing (Svg, svg, line, g)
import Svg.Attributes exposing ( x, y, width, height
                               , cx, cy, r
                               , x1, y1, x2, y2
                               , fill, stroke, strokeWidth, fontSize, transform
                               , fillOpacity, opacity, textAnchor, dominantBaseline
                               )
import Svg.Events exposing ( onClick )
import String
import List.Extra as LE
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
    , ("D5",["C3","D4","D6","5"])
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
        , placement = Nothing
        , players = Nothing
        , playerNames = []
        , playerNumber = Nothing
        , resolver = Nothing
        }

circle : String -> String -> Svg msg
circle center radius =
    Svg.circle [ cx center, cy center, r radius ]
        []

render : Maybe StonePile -> DisplayList -> RenderInfo -> Html Msg
render selectedPile list info =
    let sizes = info.sizes
        indent = 20
        is = toString indent
        iw = sizes.diameter + indent + indent
        w = toString (iw)
        h = toString (iw+10)
        c = toString sizes.center
        r = toString sizes.radius
        rb = toString sizes.bRadius
        rc = toString sizes.cRadius
        rd = toString sizes.dRadius
    in
        svg [ width w, height h ]
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
                      , renderLines info
                      , renderPoints selectedPile info
                      , renderStones selectedPile list info
                      , renderNames info
                      ]
            ]

needsResolutionColor : String
needsResolutionColor =
    "red"

sourceColor : String
sourceColor =
    "orange"

targetColor : String
targetColor =
    "green"

placementColor : String
placementColor =
    "orange"

isNodeInPileResolutions : String -> Maybe StonePile -> Bool
isNodeInPileResolutions nodeName selectedPile =
    case selectedPile of
        Nothing ->
            False
        Just pile ->
            case findResolution nodeName pile of
                Nothing ->
                    False
                Just _ ->
                    True

findResolution : String -> StonePile -> Maybe Move
findResolution nodeName pile =
    case pile.resolutions of
        [] ->
            Nothing
        res ->
            case LE.find (\r ->
                              case r of
                                  Resolution _ _ n ->
                                      if n == nodeName then
                                          True
                                      else
                                          False
                                  _ ->
                                      False
                         )
                         res
            of
                Just move ->
                    Just move
                Nothing ->
                    Nothing
                                          
playerNodes : List (Int, List (Int, List (String, (Int, Int))))
playerNodes =
    [ ( 2
      , [ ( 1
          , [ ("B", (1, 2))
            , ("C", (1, 4))
            , ("D", (1, 8))
            ]
          )
        , ( 2
          , [ ("B", (3, 4))
            , ("C", (5, 8))
            , ("D", (9, 16))
            ]
          )
        ]
      )
    , ( 4
      , [ ( 1
          , [ ("B", (1, 1))
            , ("C", (1, 2))
            , ("D", (1, 4))
            ]
          )
        , ( 2
          , [ ("B", (2, 2))
            , ("C", (3, 4))
            , ("D", (5, 8))
            ]
          )
        , ( 3
          , [ ("B", (3, 3))
            , ("C", (5, 6))
            , ("D", (9, 12))
            ]
          )
        , ( 4
          , [ ("B", (4, 4))
            , ("C", (7, 8))
            , ("D", (13, 16))
            ]
          )
        ]
      )
    ]

isPlayerNode : Int -> Int -> String -> Int -> Bool
isPlayerNode players playerNumber circle digit =
    case LE.find (\(ps, _) -> ps == players) playerNodes of
        Nothing ->
            False
        Just (_, pnodes) ->
            case LE.find (\(p, _) -> p == playerNumber) pnodes of
                Nothing ->
                    False
                Just (_, cnodes) ->
                    case LE.find (\(c, _) -> c == circle) cnodes of
                        Nothing ->
                            False
                        Just (_, (min, max)) ->
                            (digit >= min) && (digit <= max)

playerNodeColor : String
playerNodeColor =
    "green"

resolverNodeColor : String
resolverNodeColor =
    "red"

otherNodeColor : String
otherNodeColor =
    "black"

nodeColor : String -> RenderInfo -> String
nodeColor nodeName info =
    case info.players of
        Nothing ->
            otherNodeColor
        Just players ->
            case info.playerNumber of
                Nothing -> otherNodeColor
                Just playerNumber ->
                    case parseNodeName nodeName of
                        Err _ ->
                            otherNodeColor
                        Ok (circle, i) ->
                            let c = if circle == "" then "D" else circle
                            in
                                if isPlayerNode players playerNumber c i then
                                    playerNodeColor
                                else
                                    case if players == 4 then
                                             info.resolver
                                         else
                                             Nothing
                                    of
                                        Nothing ->
                                            otherNodeColor
                                        Just resolver ->
                                            if isPlayerNode players resolver c i then
                                                resolverNodeColor
                                            else
                                                otherNodeColor

pileStrokeWidth : String
pileStrokeWidth =
    "3"

renderPoints : Maybe StonePile -> RenderInfo -> List (Svg Msg)
renderPoints selectedPile info =
    let sizes = info.sizes
        sr = toString sizes.stoneRadius
        placementNode = case info.placement of
                            Just (Placement _ n) ->
                                n
                            _ ->
                                ""
        draw = (\(c, p) ->
                    let x = toString p.x
                        y = toString p.y
                        op = if isNodeInPileResolutions c selectedPile then
                                 "1"
                             else
                                 "0"
                        color = nodeColor c info
                        placements =
                            if placementNode /= c then
                                []
                            else
                                [ Svg.circle [cx x, cy y, r sr
                                             , fillOpacity "0.4"
                                             , opacity "1"
                                             , stroke placementColor
                                             , fill placementColor
                                             ]
                                      []
                                ]                                            
                    in
                        List.append placements
                            [ Svg.circle [cx x, cy y, r "5", fillOpacity "1"
                                         , fill color
                                         , stroke color
                                         ]
                                  []
                            , Svg.circle
                                [cx x, cy y, r sr, fillOpacity "0"
                                , opacity op
                                , stroke targetColor
                                , strokeWidth pileStrokeWidth
                                , onClick <| NodeClick c
                                ]
                                  []
                            ]
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
            [ Dict.toList info.locations
              |> List.concatMap draw
            , Dict.toList info.textLocations
              |> List.map drawText
            ]

lowerStoneYDelta : Int
lowerStoneYDelta =
    10

nameLocation : Int -> RenderInfo -> Maybe (Point, String)
nameLocation number info =
    case info.players of
        Nothing ->
            Nothing
        Just players ->
            let twoPlayers = (players == 2)
                top = twoPlayers || number == 1 || number == 4
                left = if twoPlayers then
                           number == 2
                       else
                           number == 3 || number == 4
                sizes = info.sizes
                c = sizes.center
                r = toFloat sizes.radius
                x = if left then
                        c - (round (0.6 * r))
                    else
                        c + (round (0.6 * r))
                y = if top then
                        c - (round (0.9 * r))
                    else
                        c + (round (0.9 * r))
            in
                Just ( { x = x, y = y }
                     , if left then "end" else "start"
                     )

renderName : Int -> RenderInfo -> List (Svg Msg)
renderName number info =
    case nameLocation number info of
        Nothing ->
            []
        Just (p, anchor) ->
            let name = case LE.find (\(n, _) -> n == number) info.playerNames
                       of
                           Just (_, name) ->
                               name
                           Nothing ->
                               "Player " ++ (toString number)
            in
                [ Svg.text_
                      [ x <| toString p.x
                      , y <| toString p.y
                      , textAnchor anchor
                      , dominantBaseline "central"
                      ]
                      [ Svg.text name ]
                ]

renderNames : RenderInfo -> List (Svg Msg)
renderNames info =
    case info.players of
        Nothing ->
            []
        Just players ->
            List.concatMap (\number -> renderName number info)
                <| if players == 2 then
                       [1, 2]
                   else
                       [1, 2, 3, 4]

renderStones : Maybe StonePile -> DisplayList -> RenderInfo -> List (Svg Msg)
renderStones selectedPile list info =
    let sizes = info.sizes
        sr = toString sizes.stoneRadius
        locs = info.locations
        slocs = info.stoneLocations
        delta = lowerStoneYDelta
        drawStone : Int -> Int -> String -> Maybe String -> Msg -> Svg Msg
        drawStone = (\x y color outline msg ->
                         Svg.circle [ cx (toString x)
                                    , cy (toString y)
                                    , r sr
                                    , fillOpacity "1"
                                    , fill color
                                    , stroke
                                          <| Maybe.withDefault "darkgray" outline
                                    , strokeWidth pileStrokeWidth
                                    , onClick msg
                                    ]
                             []
                    )
        drawPile : StonePile -> List (Svg Msg)
        drawPile = (\pile ->
                        let outline = case selectedPile of
                                          Nothing ->
                                              if pile.resolutions == [] then
                                                  Nothing
                                              else
                                                  Just needsResolutionColor
                                          Just sp ->
                                              if sp == pile then
                                                  Just sourceColor
                                              else if isNodeInPileResolutions
                                                        pile.nodeName
                                                        selectedPile
                                                   then
                                                       Just targetColor
                                                   else
                                                       Nothing
                            p = pile.location
                            msg = PileClick pile
                        in
                            case pile.colors of
                                [] ->
                                    []
                                [ stone ] ->
                                    [ drawStone p.x p.y stone outline msg ]
                                s1 :: s2 :: _ ->
                                    [ drawStone p.x (p.y + delta) s1 outline msg
                                    , drawStone p.x p.y s2 outline msg
                                    ]
                   )
    in
        List.concatMap drawPile list.allPiles

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

renderLines : RenderInfo -> List (Svg msg)
renderLines info =
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

colorLetter : Color -> String
colorLetter color =
    case color of
        White -> "W"
        Black -> "B"

placementText : Move -> String
placementText move =
    case move of
        Placement color node ->
            (colorLetter color) ++ node
        _ ->
            ""

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

deltaStones : Color -> Int -> Node -> Node
deltaStones color delta node =
    case color of
        White ->
            { node | whiteStones = max 0 (node.whiteStones + delta) }
        Black ->
            { node | blackStones = max 0 (node.blackStones + delta) }

moveStones : MovedStone -> Int -> Node -> Node
moveStones moved delta node =
    case moved of
        MoveWhite ->
            { node | whiteStones = node.whiteStones + delta }
        MoveBlack ->
            { node | blackStones = node.blackStones + delta }
        MoveBlock ->
            { node
                | whiteStones = node.whiteStones + delta
                , blackStones = node.blackStones + delta
            }

undoMove : Move -> Board -> Board
undoMove move board =
    case move of
        Placement color nodeName ->
            case getNode nodeName board of
                Nothing ->
                    board
                Just node ->
                    let n = deltaStones color -1 node
                    in
                        setNode nodeName n board
        Resolution moved from to ->
            makeMove (Resolution moved to from) board

makeMove : Move -> Board -> Board
makeMove move board =
    case move of
        Placement color nodeName ->
            case getNode nodeName board of
                Nothing ->
                    board
                Just node ->
                    let n = deltaStones color 1 node
                    in
                        setNode nodeName n board
        Resolution moved from to ->
            case getNode from board of
                Nothing ->
                    board
                Just fromNode ->
                    case getNode to board of
                        Nothing ->
                            board
                        Just toNode ->
                            let fn = moveStones moved -1 fromNode
                                tn = moveStones moved 1 toNode
                            in
                                setNode from fn
                                    <| setNode to tn board

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

movesAwayList : List (String, List (String, Maybe String, List String))
movesAwayList =
    [ ("A1", [ ("B1",  Just "B3", ["B2", "B4"])
             , ("B2",  Just "B4", ["B1", "B3"])
             , ("B3",  Just "B1", ["B2", "B4"])
             , ("B4",  Just "B2", ["B1", "B3"])
             ]
      )
    , ("B1", [ ("C1", Just "A1", ["B2", "B4"])
             , ("A1", Just "C1", ["B2", "B4"])
             , ("B2", Just "B4", ["A1", "C1"])
             , ("B4", Just "B2", ["A1", "C1"])
             ]
      )
    , ("B2", [ ("C3", Just "A1", ["B1", "B3"])
             , ("A1", Just "C3", ["B1", "B3"])
             , ("B1", Just "B3", ["A1", "C3"])
             , ("B3", Just "B1", ["A1", "C3"])
             ]
      )
    , ("B3", [ ("C5", Just "A1", ["B2", "B4"])
             , ("A1", Just "C5", ["B2", "B4"])
             , ("B2", Just "B4", ["A1", "C5"])
             , ("B4", Just "B2", ["A1", "C5"])
             ]
      )
    , ("B4", [ ("C7", Just "A1", ["B3", "B1"])
             , ("A1", Just "C7", ["B3", "B1"])
             , ("B3", Just "B1", ["A1", "C7"])
             , ("B1", Just "B3", ["A1", "C7"])
             ]
      )
    , ("C1", [ ("D1", Just "B1", ["C2", "C8"])
             , ("B1", Just "D1", ["C2", "C8"])
             , ("C2", Just "C8", ["B1", "D1"])
             , ("C8", Just "C2", ["B1", "D1"])
             ]
      )
    , ("C2", [ ("D3", Nothing, ["C1", "C3"])
             , ("C1", Just "C3", ["D3"])
             , ("C3", Just "C1", ["D3"])
             ]
      )
    , ("C3", [ ("D5", Just "B2", ["C2", "C4"])
             , ("B2", Just "D5", ["C2", "C4"])
             , ("C2", Just "C4", ["B2", "D5"])
             , ("C4", Just "C2", ["B2", "D5"])
             ]
      )
    , ("C4", [ ("D7", Nothing, ["C3", "C5"])
             , ("C3", Just "C5", ["D7"])
             , ("C5", Just "C3", ["D7"])
             ]
      )
    , ("C5", [ ("D9", Just "B3", ["C4", "C6"])
             , ("B3", Just "D9", ["C4", "C6"])
             , ("C4", Just "C6", ["B3", "D9"])
             , ("C6", Just "C4", ["B3", "D9"])
             ]
      )
    , ("C6", [ ("D11", Nothing, ["C5", "C7"])
             , ("C5", Just "C7", ["D11"])
             , ("C7", Just "C5", ["D11"])
             ]
      )
    , ("C7", [ ("D13", Just "B4", ["C6", "C8"])
             , ("B4", Just "D13", ["C6", "C8"])
             , ("C6", Just "C8", ["B4", "D13"])
             , ("C8", Just "C6", ["B4", "D13"])
             ]
      )
    , ("C8", [ ("D15", Nothing, ["C1", "C7"])
             , ("C1", Just "C7", ["D15"])
             , ("C7", Just "C1", ["D15"])
             ]
      )
    , ("D1", [ ("1", Just "C1", ["D2", "D16"])
             , ("C1", Just "1", ["D2", "D16"])
             , ("D2", Just "D16", ["1", "C1"])
             , ("D16", Just "D2", ["1", "C1"])
             ]
      )
    , ("D2", [ ("2", Nothing, ["D1", "D3"])
             , ("D1", Just "D3", ["2"])
             , ("D3", Just "D1", ["2"])
             ]
      )
    , ("D3", [ ("3", Just "C2", ["D2", "D4"])
             , ("C2", Just "3", ["D2", "D4"])
             , ("D2", Just "D4", ["3", "C2"])
             , ("D4", Just "D2", ["3", "C2"])
             ]
      )
    , ("D4", [ ("4", Nothing, ["D3", "D5"])
             , ("D3", Just "D5", ["4"])
             , ("D5", Just "D3", ["4"])
             ]
      )
    , ("D5", [ ("5", Just "C3", ["D4", "D6"])
             , ("C3", Just "5", ["D4", "D6"])
             , ("D4", Just "D6", ["5", "C3"])
             , ("D6", Just "D4", ["5", "C3"])
             ]
      )
    , ("D6", [ ("6", Nothing, ["D5", "D7"])
             , ("D5", Just "D7", ["6"])
             , ("D7", Just "D5", ["6"])
             ]
      )
    , ("D7", [ ("7", Just "C4", ["D6", "D8"])
             , ("C4", Just "7", ["D6", "D8"])
             , ("D6", Just "D8", ["7", "C4"])
             , ("D8", Just "D6", ["7", "C4"])
             ]
      )
    , ("D8", [ ("8", Nothing, ["D7", "D9"])
             , ("D7", Just "D9", ["8"])
             , ("D9", Just "D7", ["8"])
             ]
      )
    , ("D9", [ ("9", Just "C5", ["D8", "D10"])
             , ("C5", Just "9", ["D8", "D10"])
             , ("D8", Just "D10", ["9", "C5"])
             , ("D10", Just "D8", ["9", "C5"])
             ]
      )
    , ("D10", [ ("10", Nothing, ["D8", "D11"])
             , ("D9", Just "D11", ["10"])
             , ("D11", Just "D9", ["10"])
             ]
      )
    , ("D11", [ ("11", Just "C6", ["D10", "D12"])
             , ("C6", Just "11", ["D10", "D12"])
             , ("D10", Just "D12", ["11", "C6"])
             , ("D12", Just "D10", ["11", "C6"])
             ]
      )
    , ("D12", [ ("12", Nothing, ["D11", "D13"])
             , ("D11", Just "D13", ["12"])
             , ("D13", Just "D11", ["12"])
             ]
      )
    , ("D13", [ ("13", Just "C7", ["D12", "D14"])
             , ("C7", Just "13", ["D12", "D14"])
             , ("D12", Just "D14", ["13", "C7"])
             , ("D14", Just "D12", ["13", "C7"])
             ]
      )
    , ("D14", [ ("14", Nothing, ["D13", "D15"])
             , ("D13", Just "D15", ["14"])
             , ("D15", Just "D13", ["14"])
             ]
      )
    , ("D15", [ ("15", Just "C8", ["D14", "D16"])
             , ("C8", Just "15", ["D14", "D16"])
             , ("D14", Just "D16", ["15", "C8"])
             , ("D16", Just "D14", ["15", "C8"])
             ]
      )
    , ("D16", [ ("16", Nothing, ["D15", "D1"])
             , ("D15", Just "D1", ["16"])
             , ("D1", Just "D15", ["16"])
             ]
      )
    , ("1", [ ("D1", Nothing, ["16", "2"])
            , ("16", Just "2", ["D1"])
            , ("2", Just "16", ["D1"])
             ]
      )
    , ("2", [ ("D2", Nothing, ["1", "3"])
            , ("1", Just "3", ["D2"])
            , ("3", Just "1", ["D2"])
             ]
      )
    , ("3", [ ("D3", Nothing, ["2", "4"])
            , ("2", Just "4", ["D3"])
            , ("4", Just "2", ["D3"])
             ]
      )
    , ("4", [ ("D4", Nothing, ["3", "5"])
            , ("3", Just "5", ["D4"])
            , ("5", Just "3", ["D4"])
             ]
      )
    , ("5", [ ("D5", Nothing, ["4", "6"])
            , ("4", Just "6", ["D5"])
            , ("6", Just "4", ["D5"])
             ]
      )
    , ("6", [ ("D6", Nothing, ["5", "7"])
            , ("5", Just "7", ["D6"])
            , ("7", Just "5", ["D6"])
             ]
      )
    , ("7", [ ("D7", Nothing, ["6", "8"])
            , ("6", Just "8", ["D7"])
            , ("8", Just "6", ["D7"])
             ]
      )
    , ("8", [ ("D8", Nothing, ["7", "9"])
            , ("7", Just "9", ["D8"])
            , ("9", Just "7", ["D8"])
             ]
      )
    , ("9", [ ("D9", Nothing, ["8", "10"])
            , ("8", Just "10", ["D9"])
            , ("10", Just "8", ["D9"])
             ]
      )
    , ("10", [ ("D10", Nothing, ["9", "11"])
             , ("9", Just "11", ["D10"])
             , ("11", Just "9", ["D10"])
             ]
      )
    , ("11", [ ("D11", Nothing, ["10", "12"])
             , ("10", Just "12", ["D11"])
             , ("12", Just "10", ["D11"])
             ]
      )
    , ("12", [ ("D12", Nothing, ["11", "13"])
             , ("11", Just "13", ["D12"])
             , ("13", Just "11", ["D12"])
             ]
      )
    , ("13", [ ("D13", Nothing, ["12", "14"])
             , ("12", Just "14", ["D13"])
             , ("14", Just "12", ["D13"])
             ]
      )
    , ("14", [ ("D14", Nothing, ["13", "15"])
             , ("13", Just "15", ["D14"])
             , ("15", Just "13", ["D14"])
             ]
      )
    , ("15", [ ("D15", Nothing, ["14", "16"])
             , ("14", Just "16", ["D15"])
             , ("16", Just "14", ["D15"])
             ]
      )
    , ("16", [ ("D16", Nothing, ["15", "1"])
             , ("15", Just "1", ["D16"])
             , ("1", Just "15", ["D16"])
             ]
      )
    ]

movesAwayDict : Dict String (List (String, Maybe String, List String))
movesAwayDict =
    Dict.fromList movesAwayList

-- TODO: WhiteWhite, BlackBlack
classifyNeighbors : Node -> Board -> List (String, NodeClassification)
classifyNeighbors node board =
    List.map (\name ->
                  (name
                  , case Dict.get name board of
                        Nothing ->
                            Empty
                        Just n ->
                            if n.whiteStones == 0 then
                                if n.blackStones == 0 then
                                    Empty
                                else if n.blackStones == 1 then
                                    BlackOnly
                                else
                                    BlackBlack
                            else
                                if n.blackStones == 0 then
                                    if n.whiteStones == 1 then
                                        WhiteOnly
                                    else
                                        WhiteWhite
                                else
                                    Blocked
                  )
             )
             node.connections                                  

getMovesAway : String -> List (String, x, y) -> Maybe (String, x, y)
getMovesAway name tuples =
    case tuples of
        [] ->
            Nothing
        tuple :: tail ->
            let (on, _, _) = tuple
            in
                if on == name then
                    Just tuple
                else
                    getMovesAway name tail

calculateMovesAway : String -> MovedStone -> String -> List (String, NodeClassification) -> List Move
calculateMovesAway nodeName color otherName classifications =
     case Dict.get nodeName movesAwayDict of
         Nothing ->
             []
         Just tuples ->
             let sidewaysMoves = (\names res ->
                                      case names of
                                          [] ->
                                              res
                                          head :: tail ->
                                              case get head classifications of
                                                  Just Empty ->
                                                      sidewaysMoves tail
                                                          <| (Resolution
                                                                  color nodeName head
                                                             ) :: res
                                                  _ ->
                                                      sidewaysMoves tail res
                                 )
             in
                 case getMovesAway otherName tuples of
                     Nothing ->
                         []
                     Just (_, Just away, sideways) ->
                         if case get away classifications of
                                Just Empty -> True
                                Just BlackOnly -> color == MoveWhite
                                Just WhiteOnly -> color == MoveBlack
                                _ -> False
                         then
                             [ Resolution color nodeName away ]
                         else
                             sidewaysMoves sideways []
                     Just (_, _, sideways) ->
                         sidewaysMoves sideways []                         

nodePairResolutions : String -> MovedStone -> String -> NodeClassification -> List (String, NodeClassification) -> List Move
nodePairResolutions nodeName color otherName classification classifications  =
    case color of
        MoveWhite ->
            case classification of
                WhiteOnly ->
                    calculateMovesAway nodeName color otherName classifications
                BlackOnly ->
                    [ Resolution color nodeName otherName ]
                _ ->
                    []                        
        MoveBlack ->
            case classification of
                WhiteOnly ->
                    [ Resolution color nodeName otherName ]
                BlackOnly ->
                    calculateMovesAway nodeName color otherName classifications
                _ ->
                    []
        MoveBlock ->
            []

nodePairForcedResolutions : String -> MovedStone -> String -> NodeClassification -> List (String, NodeClassification) -> List Move
nodePairForcedResolutions nodeName color otherName classification classifications  =
    let res = [ Resolution color nodeName otherName ]
    in
        case color of
            MoveWhite ->
                case classification of
                    BlackOnly -> res
                    Empty -> res
                    Blocked -> []
                    _ ->        --WhiteOnly, WhiteWhite, BlackBlack
                        case LE.find (\(_, c) -> c==BlackOnly || c==Empty)
                            classifications
                        of
                            Nothing -> res
                            Just _ -> []
            MoveBlack ->
                case classification of
                    WhiteOnly -> res
                    Empty -> res
                    Blocked -> []
                    _ ->        --BlackOnly, WhiteWhite, BlackBlack
                        case LE.find (\(_, c) -> c==WhiteOnly || c==Empty)
                            classifications
                        of
                            Nothing -> res
                            Just _ -> []
            MoveBlock ->
                case classification of
                    Empty -> res
                    Blocked -> []
                    _ ->
                        case LE.find (\(_, c) -> c==Empty) classifications of
                            Nothing -> res
                            Just _ -> []

-- Implement the resolution rules.
computeResolutions : Node -> List String -> List String -> Board -> Bool -> List Move
computeResolutions node stones otherStones board hasForced =
    let classifications = classifyNeighbors node board
        nodeName = node.name
        resolutions = (\color pairResolver ->
                           List.concatMap (\(otherName, classification) ->
                                              pairResolver
                                                  nodeName color
                                                  otherName classification
                                                  classifications
                                          )
                                          classifications
                      )
    in
        if not hasForced then
            case stones of
                ["black"] ->
                    resolutions MoveBlack nodePairResolutions
                ["white"] ->
                    resolutions MoveWhite nodePairResolutions
                _ ->
                    []
        else
            case stones of
                ["black"] ->
                    if otherStones == ["black", "white"] then
                        resolutions MoveBlack nodePairForcedResolutions
                    else
                        []
                ["white"] ->
                    if otherStones == ["black", "white"] then
                        resolutions MoveWhite nodePairForcedResolutions
                    else
                        []
                ["black", "black"] ->
                    resolutions MoveBlack nodePairForcedResolutions
                ["white", "white"] ->
                    resolutions MoveWhite nodePairForcedResolutions
                ["black", "white"] ->
                    if otherStones == ["black", "white"] then
                        resolutions MoveBlock nodePairForcedResolutions
                    else
                        []
                _ ->
                    []

uniqueMoves : List Move -> List Move
uniqueMoves moves =
    let loop = (\ms res ->
                    case ms of
                        [] ->
                            List.reverse res
                        head :: tail ->
                            if List.member head res then
                                loop tail res
                            else
                                loop tail (head :: res)
               )
    in
        loop moves []

hasForcedResolutions : Node -> Bool
hasForcedResolutions node =
    node.whiteStones > 1 || node.blackStones > 1

computeDisplayList : Board -> RenderInfo -> DisplayList
computeDisplayList board info =
    let sizes = info.sizes
        sr = toString sizes.stoneRadius
        locs = info.locations
        slocs = info.stoneLocations
        nodes = Dict.toList board |> List.map Tuple.second
        hasForced = case LE.find hasForcedResolutions nodes of
                        Nothing -> False
                        Just _ -> True
        -- TODO: if there are any forced resolutions, don't process unforced.
        drawPile : Node -> Point -> List String -> List String -> StonePile
        drawPile = (\node p stones otherStones ->
                        let resolutions = computeResolutions
                                          node stones otherStones board hasForced
                            maybeRes = if resolutions == [] then
                                           []
                                       else
                                           uniqueMoves resolutions
                        in
                            case stones of
                                [] ->
                                    emptyStonePile
                                _ ->
                                    { nodeName = node.name
                                    , colors = stones
                                    , location = p
                                    , resolutions = maybeRes
                                    }
                   )
        drawNode : Node -> List StonePile
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
                                      [ drawPile node p stones [] ]
                                  s1 :: s2 :: _ ->
                                      [ drawPile node sp1 s1 []
                                      , drawPile node sp2 s2 s1
                                      ]
                   )
        allPiles = List.concatMap drawNode nodes
        unresolved = List.filter (\pile -> pile.resolutions /= []) allPiles
    in
        { allPiles = allPiles
        , unresolvedPiles = unresolved
        }

nodeToString : Node -> String
nodeToString node =
    let ws = toString node.whiteStones
        bs = toString node.blackStones
    in
        if (String.length ws) == 1
            && (String.length bs) == 1
        then
            ws ++ bs
        else
            -- This shouldn't happen
            "[" ++ ws ++ "," ++ bs ++ "]"

boardToString : Board -> String
boardToString board =
    let list = Dict.foldl (\name node res ->
                               (nodeToString node) :: res
                          )
               [] board
    in
        String.concat list

canResolve : Board -> RenderInfo -> Maybe (List StonePile) -> Bool
canResolve board info unresolvedPiles =
    let tryBoard : Board -> Set String -> (Bool, Set String)
        tryBoard = (\b bs ->
                        let s = boardToString b
                        in
                            if Set.member s bs then
                                (False, bs)
                            else
                                let bs2 = Set.insert s bs
                                    dl = computeDisplayList b info
                                in
                                    loop b dl.unresolvedPiles bs2
                   )
        tryResolutions : Board -> Set String -> List StonePile -> (Bool, Set String)
        tryResolutions =
            (\brd brds piles ->
                 if piles == [] then
                     (False, brds)
                 else
                     let lp = (\b bs mvs ->
                                   case mvs of
                                       [] ->
                                           (False, bs)
                                       move :: tail ->
                                           case tryBoard (makeMove move b) bs
                                           of
                                               (False, bs2) ->
                                                   lp b bs2 tail
                                               res ->
                                                   res
                              )
                         moves = List.foldl (\p l -> List.append p.resolutions l)
                                 [] piles
                     in
                         lp brd brds moves
            )
        loop : Board -> (List StonePile) -> Set String -> (Bool, Set String)
        loop =
            (\board piles boards ->
                 case piles of
                     [] ->
                         (True, boards)
                     _ ->
                         tryResolutions board boards piles
            )
        piles = case unresolvedPiles of
                    Just p -> p
                    Nothing -> computeDisplayList board info
                                      |> .unresolvedPiles
    in
        loop board piles (Set.singleton <| boardToString board)
            |> Tuple.first

makePlacements : Board -> List String -> Board
makePlacements board placements =
    let place = (\placement brd ->
                     case parsePlacementMove placement of
                         Err _ ->
                             brd
                         Ok move ->
                             makeMove move brd
                )
    in
        List.foldl place board placements

playerHomeSpokes : Int -> Int -> Maybe (List Int)
playerHomeSpokes player players =
    let dict = if players == 2 then
                   twoPlayerSpokes
               else
                   fourPlayerSpokes
    in
        case Dict.get player dict of
            Nothing ->
                Nothing
            Just circleDict ->
                Dict.get "" circleDict

isHomeCircleFull : Int -> Board -> RenderInfo -> Bool
isHomeCircleFull player board info =
    case info.players of
        Nothing ->
            False
        Just players ->
            case playerHomeSpokes player players of
                Nothing ->
                    False
                Just spokes ->
                    areSpokesFull board spokes

areSpokesFull : Board -> List Int -> Bool
areSpokesFull board spokes =
    (areAllHomeSpokesOccupied board spokes) ||
        ((areSpokesFullFromD board spokes) &&
         (areSpokesFullFromEnds board spokes)
        )

isHomeSpokeEmpty : Board -> Int -> Bool
isHomeSpokeEmpty board spoke =
    case getNode (toString spoke) board of
        Nothing ->
            False
        Just { whiteStones, blackStones } ->
            (whiteStones == 0) && (blackStones == 0)

areAllHomeSpokesOccupied : Board -> List Int -> Bool
areAllHomeSpokesOccupied board spokes =
    case LE.find (isHomeSpokeEmpty board) spokes of
        Nothing ->
            True
        Just _ ->
            False

nodeCounts : Board -> String -> Maybe (Int, Int, Int)
nodeCounts board nodeName =
    case Dict.get nodeName board of
        Nothing ->
            Nothing
        Just node ->
            Just <|
                ( node.whiteStones
                , node.blackStones
                , node.whiteStones + node.blackStones
                )

fullNodeCounts : Board -> String -> (Int, Int, Int)
fullNodeCounts board node =
    Maybe.withDefault (1, 1, 2) <| nodeCounts board node

emptyNodeCounts : Board -> String -> (Int, Int, Int)
emptyNodeCounts board node =
    Maybe.withDefault (0, 0, 0) <| nodeCounts board node

canFillWith : Board -> Color -> String -> Int -> Bool
canFillWith board color nodeName spoke =
    True

hasLiveNonhomeNeighbor:  Board -> String -> List (String, NodeClassification) -> Bool
hasLiveNonhomeNeighbor board homeNode classifications =
    let loop = (\cs ->
                    case cs of
                        [] ->
                            False
                        (n, c) :: tail ->
                            if (n /= homeNode) &&
                                (c==Empty || c==WhiteOnly || c==BlackOnly)
                            then
                                True
                            else
                                loop tail
               )
    in
        loop classifications    

hasEmptyNonhomeNeighbor:  Board -> String -> List (String, NodeClassification) -> Bool
hasEmptyNonhomeNeighbor board homeNode classifications =
    let loop = (\cs ->
                    case cs of
                        [] ->
                            False
                        (n, c) :: tail ->
                            if (n /= homeNode) && (c==Empty)
                            then
                                True
                            else
                                loop tail
               )
    in
        loop classifications

canPushDToHomeCircle : Board -> String -> Bool
canPushDToHomeCircle board spoke =
    let dsp = "D" ++ spoke
    in
        case getNode dsp board of
            Just node ->
                let classifications = classifyNeighbors node board
                    dt = node.blackStones + node.whiteStones
                in
                    (dt==0 &&
                         hasLiveNonhomeNeighbor
                         board spoke classifications)
                    ||
                    (dt==1 &&
                         hasEmptyNonhomeNeighbor
                         board spoke classifications)
            Nothing ->
                False

isNodeEmpty : Board -> String -> Bool
isNodeEmpty board nodeName =
    case getNode nodeName board of
        Nothing ->
            False
        Just node ->
            0 == (node.whiteStones + node.blackStones)

canFillSpokeFromD : Board -> Int -> Bool
canFillSpokeFromD board spoke =
    let sp = toString spoke
    in
        if isNodeEmpty board sp then
            canPushDToHomeCircle board sp
        else
            False

areSpokesFullFromD : Board -> List Int -> Bool
areSpokesFullFromD board spokes =
    case LE.find (canFillSpokeFromD board) spokes of
        Just _ ->
            False
        Nothing ->
            True

nextHomeSpoke : Int -> Int -> Int
nextHomeSpoke spoke direction =
    if direction > 0 then
        (spoke % 16) + 1
    else
        if spoke == 1 then
            16
        else
            spoke - 1
        
areSpokesFullFromEnds : Board -> List Int -> Bool
areSpokesFullFromEnds board spokes =
    case List.head spokes of
        Nothing ->
            False               --can't happen
        Just first ->
            case LE.last spokes of
                Nothing ->
                    False --can't happen
                Just last ->
                    let beforeFirst = nextHomeSpoke first (-1)
                        afterLast = nextHomeSpoke last 1
                        (isFull, lastLooked) =
                            isSpokeFullFromEnd board first beforeFirst afterLast (-1)
                    in
                        if isFull && (lastLooked /= afterLast) then
                            let (res, _) =
                                isSpokeFullFromEnd board last afterLast beforeFirst 1
                            in
                                res
                        else
                            False
                                
isSpokeFullFromEnd : Board -> Int -> Int -> Int -> Int -> (Bool, Int)
isSpokeFullFromEnd board first afterFirst last direction =
    if not <| isNodeEmpty board (toString first) then
        (True, first)
    else
        let (notres, lastLooked) = canFillFromEnd board afterFirst last direction
        in
            (not notres, lastLooked)

canFillFromEnd : Board -> Int -> Int -> Int -> (Bool, Int)
canFillFromEnd board first last direction =
    let sp = toString first
    in
        case getNode sp board of
            Nothing ->
                (False, first) --can't happen
            Just node ->
                case node.whiteStones + node.blackStones of
                    0 ->
                        if canPushDToHomeCircle board sp then
                            (True, first)
                        else if first == last then
                            (False, first)
                        else
                            canFillFromEnd
                                board (nextHomeSpoke first direction) last direction
                    1 ->
                        if first == last then
                            (False, first)
                        else
                            canFillWithColorFromEnd
                                board (if node.whiteStones == 1 then
                                           White
                                       else
                                           Black
                                      )
                                (nextHomeSpoke first direction)
                                last direction
                    _ ->
                        (False, first)
                            
            
canFillWithColorFromEnd : Board -> Color -> Int -> Int -> Int -> (Bool, Int)
canFillWithColorFromEnd board color first last direction =
    let sp = toString first
    in
        case getNode sp board of
            Nothing ->
                (False, first) --can't happen
            Just node ->
                case node.whiteStones + node.blackStones of
                    0 ->
                        if canPushDToHomeCircle board sp then
                            -- It might be the wrong color. Maybe fix later.
                            (True, first)
                        else if first == last then
                            (False, first)
                        else
                            canFillWithColorFromEnd
                                board color (nextHomeSpoke first direction)
                                last direction
                    1 ->
                        let otherColor = (if node.whiteStones == 1 then
                                              White
                                          else
                                              Black
                                         )
                        in
                            -- If unequal, the other could be pulled away,
                            -- and a same-color stone pushed in from D circle.
                            -- Maybe fix later.
                            (color == otherColor, first)
                    _ ->
                        (False, first)
