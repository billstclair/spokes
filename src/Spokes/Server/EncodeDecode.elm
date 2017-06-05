----------------------------------------------------------------------
--
-- EncodeDecode.elm
-- JSON encoder and decoder for Spokes server wire protocol.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Spokes.Server.EncodeDecode exposing ( messageDecoder, decodeMessage
                                           , messageEncoder, encodeMessage
                                           )

import Spokes.Types exposing ( Color(..), Move(..), Message(..)
                             , movedStoneString, stringToMovedStone
                             , get
                             )
import Spokes.Board exposing ( parsePlacementMove, placementText, colorLetter )

import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE exposing ( Value )

---
--- Decoder
---

rawMessageDecoder : Decoder Message
rawMessageDecoder =
    JD.map3 RawMessage
        (JD.index 0 JD.string)
        (JD.index 1 JD.string)
        (JD.index 2 (JD.keyValuePairs JD.string))

type alias MessageParams =
    { req : Maybe String
    , rsp : Maybe String
    , players : Maybe Int
    , gameid : Maybe String
    , name : Maybe String
    , number : Maybe Int
    , turn : Maybe Int
    , resolver : Maybe Int
    , placement : Maybe Move
    , placements : Maybe (List Move)
    , resolution : Maybe Move
    , message : Maybe Message
    , request : Maybe String
    , id : Maybe Int
    , text : Maybe String
    }

rawMessageToParams : Message -> Maybe MessageParams
rawMessageToParams message =
    case message of
        RawMessage typ msg plist ->
            Just { req = if typ == "req" then Just msg else Nothing
                 , rsp = if typ == "rsp" then Just msg else Nothing
                 , players = maybeInt <| get "players" plist
                 , gameid = get "gameid" plist
                 , name = get "name" plist
                 , number = maybeInt <| get "number" plist
                 , turn = maybeInt <| get "turn" plist
                 , resolver = maybeInt <| get "resolver" plist
                 , placement = maybePlacement <| get "placement" plist
                 , placements = maybePlacements <| get "placements" plist
                 , resolution = maybeResolution plist
                 , message = maybeMessage <| get "message" plist
                 , request = get "request" plist
                 , id = maybeInt <| get "id" plist
                 , text = get "text" plist
                 }
        _ ->
            Nothing

maybeInt : Maybe String -> Maybe Int
maybeInt ms =
    case ms of
        Nothing ->
            Nothing
        Just s ->
            case String.toInt s of
                Ok i ->
                    Just i
                Err _ ->
                    Nothing

maybePlacement : Maybe String -> Maybe Move
maybePlacement ms =
    case ms of
        Nothing ->
            Nothing
        Just s ->
            case parsePlacementMove s of
                Ok placement ->
                    Just placement
                _ ->
                    Nothing

maybePlacements : Maybe String -> Maybe (List Move)
maybePlacements ms =
    case ms of
        Nothing ->
            Nothing
        Just s ->
            let strings = String.split "," s
                placements = List.map parsePlacementMove strings
                loop = (\ps res ->
                            case ps of
                                [] ->
                                    Just <| List.reverse res
                                (Ok p) :: tail ->
                                    loop tail <| p :: res
                                _ ->
                                    Nothing
                       )
            in
                loop placements []                

maybeResolution : List (String, String) -> Maybe Move
maybeResolution plist =
    case (get "color" plist, get "from" plist, get "to" plist) of
        (Just color, Just from, Just to) ->
            case stringToMovedStone color of
                Nothing ->
                    Nothing
                Just stone ->
                    Just <| Resolution stone from to
        _ ->
            Nothing

maybeMessage : Maybe String -> Maybe Message
maybeMessage ms =
    case ms of
        Nothing ->
            Nothing
        Just s ->
            case decodeMessage s of
                Err _ ->
                    Nothing
                Ok m ->
                    Just m            

decodeMessage : String -> Result String Message
decodeMessage string =
    JD.decodeString messageDecoder string

messageDecoder : Decoder Message
messageDecoder =
    JD.lazy (\_ -> JD.map parseRawMessage rawMessageDecoder)

allStrings : List (Maybe String) -> Maybe (List String)
allStrings strings =
    let loop = (\list res ->
                    case list of
                        [] ->
                            Just <| List.reverse res
                        (Just e) :: tail ->
                            loop tail (e :: res)
                        _ ->
                            Nothing
               )
    in
        loop strings []

parseRequest : String -> MessageParams -> Message -> Message
parseRequest msg params rawMessage =
    case msg of
        "new" ->
            let { players, name } = params
                p = case players of
                        Nothing -> 2
                        Just ps -> ps
            in
                case name of
                    Nothing ->
                        rawMessage
                    Just n ->
                        NewReq { players = p, name = n }
        "join" ->
            let { gameid, name } = params
            in
                case allStrings [ gameid, name ] of
                    Just [gid, n] ->
                        JoinReq { gameid = gid, name = n }
                    _ ->
                        rawMessage
        "place" ->
            let { gameid, placement, number } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case placement of
                            Nothing ->
                                rawMessage
                            Just p ->
                                case number of
                                    Nothing ->
                                        rawMessage
                                    Just num ->
                                        PlaceReq { gameid = gid
                                                 , placement = p
                                                 , number = num
                                         }
        "resolve" ->
            let { gameid, resolution } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case resolution of
                            Nothing ->
                                rawMessage
                            Just res ->
                                ResolveReq { gameid = gid
                                           , resolution = res
                                           }
        "undo" ->
            let { gameid, message } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case message of
                            Nothing ->
                                rawMessage
                            Just mes ->
                                UndoReq { gameid = gid
                                        , message = mes
                                        }
        "chat" ->
            let { gameid, text, number } = params
            in
                case number of
                    Just num ->
                        case allStrings [ gameid, text ] of
                            Just [ gid, tex ] ->
                                ChatReq { gameid = gid, text = tex, number = num }
                            _ ->
                                rawMessage
                    _ ->
                        rawMessage
        _ ->
            rawMessage

parseResponse : String -> MessageParams -> Message -> Message
parseResponse msg params rawMessage =
    case msg of
        "new" ->
            let { gameid, players, name } = params
            in
                case allStrings [gameid, name] of
                    Just [gid, n] ->
                        case players of
                            Nothing ->
                                rawMessage
                            Just p ->
                                NewRsp { gameid = gid
                                       , players = p
                                       , name = n
                                       }
                    _ ->
                        rawMessage
        "join" ->
            let { gameid, name, number } = params
            in
                case number of
                    Nothing ->
                        rawMessage
                    Just num ->
                        case allStrings [ gameid, name ] of
                            Just [gid, n] ->
                                JoinRsp { gameid = gid
                                        , name = n
                                        , number = num}
                            _ ->
                                rawMessage
        "place" ->
            let { gameid, number } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case number of
                            Nothing ->
                                rawMessage
                            Just num ->
                                PlaceRsp { gameid = gid
                                         , number = num
                                         }
        "placed" ->
            let { gameid, placements } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case placements of
                            Nothing ->
                                rawMessage
                            Just ps ->
                                PlacedRsp { gameid = gid
                                          , placements = ps
                                          }
        "resolve" ->
            let { gameid, resolution } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case resolution of
                            Nothing ->
                                rawMessage
                            Just res ->
                                ResolveRsp { gameid = gid
                                           , resolution = res
                                           }
        "undo" ->
            let { gameid, message } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case message of
                            Nothing ->
                                rawMessage
                            Just mes ->
                                UndoRsp { gameid = gid
                                        , message = mes
                                        }
        "error" ->
            let { request, id, text } = params
            in
                case id of
                    Nothing ->
                        rawMessage
                    Just i ->
                        case allStrings [ request, text ] of
                            Just [ req, txt ] ->
                                ErrorRsp { request = req
                                         , id = i
                                         , text = txt
                                         }
                            _ ->
                                rawMessage
        "chat" ->
            let { gameid, text, number } = params
            in
                case number of
                    Nothing ->
                        rawMessage
                    Just num ->
                        case allStrings [ gameid, text ] of
                            Just [ gid, txt ] ->
                                ChatRsp { gameid = gid
                                        , text = txt
                                        , number = num
                                        }
                            _ ->
                                rawMessage
        _ ->
            rawMessage

parseRawMessage : Message -> Message
parseRawMessage rawMessage =
    case rawMessageToParams rawMessage of
        Nothing ->
            rawMessage
        Just params ->
            let { req, rsp } = params
            in
                case (req, rsp) of
                    (Just msg, Nothing) ->
                        parseRequest msg params rawMessage 
                    (Nothing, Just msg) ->
                        parseResponse msg params rawMessage
                    _ ->
                        rawMessage

---
--- Encoder
---

encodeMessage : Message -> String
encodeMessage message =
    JE.encode 0 <| messageEncoder message

messageValue : String -> String -> List (String, String) -> Value
messageValue typ msg params =
    let p = List.map (\(k, v) -> (k, JE.string v)) params
    in
        JE.list [ JE.string typ, JE.string msg, JE.object p ]

messageEncoder : Message -> Value
messageEncoder message =
    case message of
        RawMessage typ msg plist ->
            messageValue typ msg plist
        -- Basic game play
        NewReq { players, name } ->
            messageValue "req" "new" [ ("players", toString players)
                                     , ("name", name)
                                     ]
        NewRsp { gameid, players, name } ->
            messageValue "rsp" "new" [ ("gameid", gameid)
                                     , ("players", toString players)
                                     , ("name", name)
                                     ]
        JoinReq { gameid, name } ->
            messageValue "req" "join" [("gameid", gameid), ("name", name)]
        JoinRsp { gameid, name, number } ->
            messageValue "rsp" "join" [ ("gameid", gameid)
                                      , ("name", name)
                                      , ("number", toString number)
                                      ]
        PlaceReq { gameid, placement, number } ->
            messageValue "req" "place" [ ("gameid", gameid)
                                       , ("placement", placementText placement)
                                       , ("number", toString number)
                                       ]
        PlaceRsp { gameid, number } ->
            messageValue "rsp" "place" [ ("gameid", gameid)
                                       , ("number", toString number)
                                       ]
        PlacedRsp { gameid, placements } ->
            messageValue "rsp" "placed" [ ("gameid", gameid)
                                        , ("placements",
                                               String.join ","
                                               <| List.map
                                               placementText placements
                                          )
                                        ]
        ResolveReq { gameid, resolution } ->
            let (color, from, to) = resolutionToStrings resolution
            in
                messageValue "req" "resolve" [ ("gameid", gameid)
                                             , ("color", color)
                                             , ("from", from)
                                             , ("to", to)
                                             ]
        ResolveRsp { gameid, resolution } ->
            let (color, from, to) = resolutionToStrings resolution
            in
                messageValue "rsp" "resolve" [ ("gameid", gameid)
                                             , ("color", color)
                                             , ("from", from)
                                             , ("to", to)
                                             ]
        -- Errors
        UndoReq { gameid, message } ->
            messageValue "req" "undo" [ ("gameid", gameid)
                                      , ("message", encodeMessage message)
                                      ]
        UndoRsp { gameid, message } ->
            messageValue "rsp" "undo" [ ("gameid", gameid)
                                      , ("message", encodeMessage message)
                                      ]
        ErrorRsp { request, id, text } ->
            messageValue "rsp" "error" [ ("request", request)
                                       , ("id", toString id)
                                       , ("text", text)
                                       ]
        -- Chat
        ChatReq { gameid, text, number } ->
            messageValue "req" "chat" [ ("gameid", gameid)
                                      , ("text", text)
                                      , ("number", toString number)
                                      ]
        ChatRsp { gameid, text, number } ->
            messageValue "rsp" "chat" [ ("gameid", gameid)
                                      , ("text", text)
                                      , ("number", toString number)
                                      ]

resolutionToStrings : Move -> (String, String, String)
resolutionToStrings move =
    case move of
        Resolution moved from to ->
            (movedStoneString moved, from, to)
        _ ->
            ("", "", "")
