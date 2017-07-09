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
                                           , restoreStateEncoder, restoreStateDecoder
                                           , encodeRestoreState, decodeRestoreState
                                           , fixCurlyQuotes
                                           )

import Spokes.Types exposing ( Color(..), Move(..), Message(..), GameOverReason(..)
                             , PublicGames, PublicGame, RestoreState
                             , movedStoneString, stringToMovedStone
                             , get, toBitmap, fromBitmap
                             )
import Spokes.Board exposing ( parsePlacementMove, placementText, colorLetter )

import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE exposing ( Value )
import Char
import String.Extra as SE

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
    , playerid : Maybe String
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
    , games : Maybe String
    , isPublic : Bool
    , reason : Maybe GameOverReason
    , restoreState : Maybe RestoreState
    , vote : Maybe Bool
    , votedUnresolvable : Maybe Int
    }

rawMessageToParams : Message -> Maybe MessageParams
rawMessageToParams message =
    case message of
        RawMessage typ msg plist ->
            let placements = maybePlacements <| get "placements" plist
                resolution = maybeResolution plist
            in
                Just { req = if typ == "req" then Just msg else Nothing
                     , rsp = if typ == "rsp" then Just msg else Nothing
                     , players = maybeInt <| get "players" plist
                     , gameid = get "gameid" plist
                     , playerid = get "playerid" plist
                     , name = get "name" plist
                     , number = maybeInt <| get "number" plist
                     , turn = maybeInt <| get "turn" plist
                     , resolver = maybeInt <| get "resolver" plist
                     , placement = maybePlacement <| get "placement" plist
                     , placements = placements
                     , resolution = resolution
                     , message = maybeMessage <| get "message" plist
                     , request = get "request" plist
                     , id = maybeInt <| get "id" plist
                     , text = get "text" plist
                     , games = get "games" plist
                     , isPublic = Maybe.withDefault False
                                  <| maybeBool (get "isPublic" plist)
                     , reason = maybeGameOverReason
                                (get "reason" plist)
                                (get "reasonnumber" plist)
                                placements
                                resolution
                     , restoreState = maybeRestoreState
                                      <| get "restoreState" plist
                     , vote = maybeBool (get "vote" plist)
                     , votedUnresolvable = maybeInt (get "votedUnresolvable" plist)
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

maybeBool : Maybe String -> Maybe Bool
maybeBool mb =
    case mb of
        Nothing ->
            Nothing
        Just s ->
            case s of
                "true" ->
                    Just True
                "false" ->
                    Just False
                _ ->
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

stringToGameOverReason : String -> Maybe String -> Maybe (List Move) -> Maybe Move -> GameOverReason
stringToGameOverReason string number placements resolution =
    case string of
        "resignation" ->
            case number of
                Nothing -> UnknownReason string
                Just n ->
                    case String.toInt n of
                        Err _ -> UnknownReason string
                        Ok i -> ResignationReason i
        "unresolvablevote" ->
            case resolution of
                Nothing ->
                    UnknownReason string
                Just move ->
                    UnresolvableVoteReason [move]
        "unresolvable" ->
            case placements of
                Nothing ->
                    case resolution of
                        Nothing ->
                            UnknownReason string
                        Just move ->
                            UnresolvableReason [move]
                Just moves ->
                    UnresolvableReason moves
        "homecirclefull" ->
            case number of
                Nothing ->
                    UnknownReason string
                Just n ->
                    case String.toInt n of
                        Err _ ->
                            UnknownReason string
                        Ok i ->
                            case placements of
                                Nothing ->
                                    case resolution of
                                        Nothing ->
                                            UnknownReason string
                                        Just move ->
                                            HomeCircleFullReason i [move]
                                Just moves ->
                                    HomeCircleFullReason i moves
        "timeout" -> TimeoutReason
        _ -> UnknownReason string

movesToPlacementsOrResolution : List Move -> (Maybe (List Move), Maybe Move)
movesToPlacementsOrResolution moves =
    case moves of
        [move] ->
            case move of
                Resolution _ _ _ ->
                    (Nothing, Just move)
                _ ->
                    (Just moves, Nothing)
        _ ->
            (Just moves, Nothing)

gameOverReasonToString : GameOverReason -> (String, Maybe String, Maybe (List Move), Maybe Move)
gameOverReasonToString reason =
    case reason of
        ResignationReason n ->
            ("resignation", Just (toString n), Nothing, Nothing)
        UnresolvableVoteReason moves ->
            let (placements, resolution) = movesToPlacementsOrResolution moves
            in
                ("unresolvablevote", Nothing, placements, resolution)
        UnresolvableReason moves ->
            let (placements, resolution) = movesToPlacementsOrResolution moves
            in
                ("unresolvable", Nothing, placements, resolution)
        HomeCircleFullReason n moves ->
            let (placements, resolution) = movesToPlacementsOrResolution moves
            in
                ("homecirclefull", Just (toString n), placements, resolution)
        TimeoutReason ->
            ("timeout", Nothing, Nothing, Nothing)
        UnknownReason s ->
            (s, Nothing, Nothing, Nothing)

maybeGameOverReason : Maybe String -> Maybe String -> Maybe (List Move) -> Maybe Move -> Maybe GameOverReason
maybeGameOverReason gos number placements resolution =
    case gos of
        Nothing ->
            Nothing
        Just s ->
            Just <| stringToGameOverReason s number placements resolution

maybeRestoreState : Maybe String -> Maybe RestoreState
maybeRestoreState state =
    case state of
        Nothing ->
            Nothing
        Just s ->
            case decodeRestoreState s of
                Err _ ->
                    Nothing
                Ok restoreState ->
                    Just restoreState

decodeMessage : String -> Result String Message
decodeMessage string =
    JD.decodeString messageDecoder string

messageDecoder : Decoder Message
messageDecoder =
    JD.lazy (\_ -> JD.map parseRawMessage rawMessageDecoder)

publicGameDecoder : Decoder PublicGame
publicGameDecoder =
    JD.map3 PublicGame
        (JD.field "gameid" JD.string)
        (JD.field "players" JD.int)
        (JD.field "playerNames" <| JD.list JD.string)

publicGamesDecoder : Decoder PublicGames
publicGamesDecoder =
    JD.map2 PublicGames
        (JD.field "twoPlayer" <| JD.list publicGameDecoder)
        (JD.field "fourPlayer" <| JD.list publicGameDecoder)

decodePublicGames : String -> Result String PublicGames
decodePublicGames string =
    JD.decodeString publicGamesDecoder string

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
            let { players, name, isPublic, restoreState } = params
                p = case players of
                        Nothing -> 2
                        Just ps -> ps
            in
                case name of
                    Nothing ->
                        rawMessage
                    Just n ->
                        NewReq { players = p
                               , name = n
                               , isPublic = isPublic
                               , restoreState = restoreState
                               }
        "join" ->
            let { gameid, name } = params
            in
                case allStrings [ gameid, name ] of
                    Just [gid, n] ->
                        JoinReq { gameid = gid, name = n }
                    _ ->
                        rawMessage
        "place" ->
            let { playerid, placement } = params
            in
                case playerid of
                    Nothing ->
                        rawMessage
                    Just pid ->
                        case placement of
                            Nothing ->
                                rawMessage
                            Just p ->
                                PlaceReq { playerid = pid
                                         , placement = p
                                         }
        "resolve" ->
            let { playerid, resolution } = params
            in
                case playerid of
                    Nothing ->
                        rawMessage
                    Just pid ->
                        case resolution of
                            Nothing ->
                                rawMessage
                            Just res ->
                                ResolveReq { playerid = pid
                                           , resolution = res
                                           }
        "responseCount" ->
            let { playerid, number } = params
            in
                case playerid of
                    Nothing ->
                        rawMessage
                    Just pid ->
                        case number of
                            Nothing ->
                                rawMessage
                            Just n ->
                                ResponseCountReq { playerid = pid
                                                 , number = n
                                                 }
        "games" ->
            GamesReq
        "undo" ->
            let { playerid, message } = params
            in
                case playerid of
                    Nothing ->
                        rawMessage
                    Just pid ->
                        case message of
                            Nothing ->
                                rawMessage
                            Just mes ->
                                UndoReq { playerid = pid
                                        , message = mes
                                        }
        "resign" ->
            let { playerid } = params
            in
                case playerid of
                    Nothing ->
                        rawMessage
                    Just pid ->
                        ResignReq { playerid = pid }
        "unresolvableVote" ->
            let { playerid, vote } = params
            in
                case playerid of
                    Nothing ->
                        rawMessage
                    Just pid ->
                        case vote of
                            Nothing ->
                                rawMessage
                            Just v ->
                                UnresolvableVoteReq { playerid = pid
                                                    , vote = v
                                                    }
        "chat" ->
            let { playerid, text } = params
            in
                case allStrings [ playerid, text ] of
                    Just [ pid, tex ] ->
                        ChatReq { playerid = pid, text = tex }
                    _ ->
                        rawMessage
        _ ->
            rawMessage

parseResponse : String -> MessageParams -> Message -> Message
parseResponse msg params rawMessage =
    case msg of
        "new" ->
            let { gameid, playerid, players, name, restoreState } = params
            in
                case allStrings [gameid, playerid, name] of
                    Just [gid, pid, n] ->
                        case players of
                            Nothing ->
                                rawMessage
                            Just p ->
                                NewRsp { gameid = gid
                                       , playerid = pid
                                       , players = p
                                       , name = n
                                       , restoreState = restoreState
                                       }
                    _ ->
                        rawMessage
        "join" ->
            let { gameid, players, name, playerid, number, restoreState } = params
            in
                case number of
                    Nothing ->
                        rawMessage
                    Just num ->
                        case allStrings [gameid, name] of
                            Just [gid, n] ->
                                case players of
                                    Nothing ->
                                        rawMessage
                                    Just ps ->
                                        JoinRsp { gameid = gid
                                                , players = ps
                                                , name = n
                                                , playerid = playerid
                                                , number = num
                                                , restoreState = restoreState
                                                }
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
        "responseCount" ->
            let { gameid, number, restoreState, votedUnresolvable } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case number of
                            Nothing ->
                                rawMessage
                            Just n ->
                                case restoreState of
                                    Nothing ->
                                        rawMessage
                                    Just state ->
                                        case votedUnresolvable of
                                            Nothing ->
                                                rawMessage
                                            Just vr ->
                                                ResponseCountRsp
                                                { gameid = gid
                                                , number = n
                                                , restoreState = state
                                                , votedUnresolvable =
                                                    fromBitmap vr
                                                }
        "resign" ->
            let { gameid, number, placements } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case number of
                            Nothing ->
                                rawMessage
                            Just num ->
                                ResignRsp { gameid = gid
                                          , number = num
                                          , placements = placements         
                                          }
        "unresolvableVote" ->
            let { gameid, number, vote } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case number of
                            Nothing ->
                                rawMessage
                            Just n ->
                                case vote of
                                    Nothing ->
                                        rawMessage
                                    Just v ->
                                        UnresolvableVoteRsp { gameid = gid
                                                            , number = n
                                                            , vote = v
                                                            }
        "gameover" ->
            let { gameid, reason } = params
            in
                case gameid of
                    Nothing ->
                        rawMessage
                    Just gid ->
                        case reason of
                            Nothing ->
                                rawMessage
                            Just reas ->
                                GameOverRsp { gameid = gid
                                            , reason = reas
                                            }
        "games" ->
            let { games } = params
            in
                case games of
                    Nothing ->
                        rawMessage
                    Just g ->
                        case decodePublicGames g of
                            Err _ ->
                                rawMessage
                            Ok publicGames ->
                                GamesRsp publicGames
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

restoreStateDecoder : Decoder RestoreState
restoreStateDecoder =
    JD.map3 RestoreState
        (JD.index 0 JD.string)
        (JD.index 1 <| JD.list JD.string)
        (JD.index 2 JD.int)

leftCurlyQuote : String
leftCurlyQuote =
    String.fromChar(Char.fromCode(8220))

rightCurlyQuote : String
rightCurlyQuote =
    String.fromChar(Char.fromCode(8221))

asciiQuote : String
asciiQuote =
    "\""

-- Users will sometimes type restore state strings, so they'll get
-- curly quotes, thanks to helpful text editors.
fixCurlyQuotes : String -> String
fixCurlyQuotes string =
    SE.replace leftCurlyQuote asciiQuote
        <| SE.replace rightCurlyQuote asciiQuote string

decodeRestoreState : String -> Result String RestoreState
decodeRestoreState json =
    JD.decodeString restoreStateDecoder <| fixCurlyQuotes json

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

placementsString : List Move -> String
placementsString placements =
    String.join "," <| List.map placementText placements

publicGameEncoder : PublicGame -> Value
publicGameEncoder game =
    JE.object [ ("gameid", JE.string game.gameid)
              , ("players", JE.int game.players)
              , ("playerNames", JE.list <| List.map JE.string game.playerNames)
              ]

publicGamesEncoder : PublicGames -> Value
publicGamesEncoder games =
    JE.object [ ( "twoPlayer"
                , JE.list <| List.map publicGameEncoder games.twoPlayer
                )
              , ( "fourPlayer"
                , JE.list <| List.map publicGameEncoder games.fourPlayer
                )
              ]

encodePublicGames : PublicGames -> String
encodePublicGames games =
    JE.encode 0 <| publicGamesEncoder games

messageEncoder : Message -> Value
messageEncoder message =
    case message of
        RawMessage typ msg plist ->
            messageValue typ msg plist
        -- Basic game play
        NewReq { players, name, isPublic, restoreState } ->
            let isPublicPairs =
                    if isPublic then
                        [ ("isPublic", "true") ]
                    else
                        []
                rsPairs = case restoreState of
                              Nothing ->
                                  []
                              Just rs ->
                                  [ ("restoreState", encodeRestoreState rs) ]
            in
                messageValue "req" "new"
                    <| List.concat
                        [ [ ("players", toString players)
                          , ("name", name)
                          ]
                        , isPublicPairs
                        , rsPairs
                        ]
        NewRsp { gameid, playerid, players, name, restoreState } ->
            messageValue "rsp" "new"
                <| List.concat [ [ ("gameid", gameid)
                                 , ("playerid", playerid)
                                 , ("players", toString players)
                                 , ("name", name)
                                 ]
                               , case restoreState of
                                     Nothing ->
                                         []
                                     Just rs ->
                                         [ ("restoreState", encodeRestoreState rs) ]
                               ]
        JoinReq { gameid, name } ->
            messageValue "req" "join" [("gameid", gameid), ("name", name)]
        JoinRsp { gameid, players, name, playerid, number, restoreState } ->
            messageValue "rsp" "join"
                <| List.concat
                    [ [ ("gameid", gameid)
                      , ("players", toString players)
                      , ("name", name)
                      ]
                    , case playerid of
                          Nothing -> []
                          Just pid -> [ ("playerid", pid) ]
                    , [ ("number", toString number) ]
                    , case restoreState of
                          Nothing -> []
                          Just rs -> [ ("restoreState", encodeRestoreState rs) ]
                    ]
        PlaceReq { playerid, placement } ->
            messageValue "req" "place" [ ("playerid", playerid)
                                       , ("placement", placementText placement)
                                       ]
        PlaceRsp { gameid, number } ->
            messageValue "rsp" "place" [ ("gameid", gameid)
                                       , ("number", toString number)
                                       ]
        PlacedRsp { gameid, placements } ->
            messageValue "rsp" "placed" [ ("gameid", gameid)
                                        , ("placements", placementsString placements)
                                        ]
        ResolveReq { playerid, resolution } ->
            let (color, from, to) = resolutionToStrings resolution
            in
                messageValue "req" "resolve" [ ("playerid", playerid)
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
        ResponseCountReq { playerid, number } ->
            messageValue "req" "responseCount"  [ ("playerid", playerid)
                                                , ("number", toString number)
                                                ]
        ResponseCountRsp { gameid, number, restoreState, votedUnresolvable } ->
            messageValue "rsp" "responseCount" [ ("gameid", gameid)
                                               , ("number", toString number)
                                               , ( "restoreState"
                                                 , encodeRestoreState restoreState
                                                 )
                                               , ( "votedUnresolvable"
                                                 , toString
                                                     <| toBitmap votedUnresolvable
                                                 )
                                               ]
        -- Public games
        GamesReq ->
            messageValue "req" "games" []
        GamesRsp games ->
            messageValue "rsp" "games" [ ("games", encodePublicGames games) ]
        -- End of game
        ResignReq { playerid } ->
            messageValue "req" "resign" [ ("playerid", playerid) ]
        ResignRsp { gameid, number, placements } ->
            let placementParams =
                    case placements of
                        Nothing ->
                            []
                        Just ps ->
                            [ ("placements", placementsString ps) ]
            in
                messageValue "rsp" "resign"
                    <| List.append [ ("gameid", gameid)
                                   , ("number", toString number)
                                   ]
                        placementParams
        UnresolvableVoteReq { playerid, vote } ->
            messageValue "req" "unresolvableVote"
                [ ("playerid", playerid)
                , ("vote", if vote then "true" else "false")
                ]
        UnresolvableVoteRsp { gameid, number, vote } ->
            messageValue "rsp" "unresolvableVote"
                [ ("gameid", gameid)
                , ("number", toString number)
                , ("vote", if vote then "true" else "false")
                ]
        GameOverRsp { gameid, reason } ->
            let (s, number, placements, resolution) = gameOverReasonToString reason
                params = case placements of
                             Nothing ->
                                 case resolution of
                                     Nothing ->
                                         [ ("gameid", gameid)
                                         , ("reason", s)
                                         ]
                                     Just move ->
                                         let (color, from, to) =
                                                 resolutionToStrings move
                                         in
                                             [ ("gameid", gameid)
                                             , ("reason", s)
                                             , ("color", color)
                                             , ("from", from)
                                             , ("to", to)
                                             ]
                             Just moves ->
                                 [ ("gameid", gameid)
                                 , ("reason", s)
                                 , ("placements", placementsString moves)
                                 ]
                params2 = case number of
                              Nothing ->
                                  params
                              Just i -> ("reasonnumber", i) :: params
            in
                messageValue "rsp" "gameover" params2
        -- Errors
        UndoReq { playerid, message } ->
            messageValue "req" "undo" [ ("playerid", playerid)
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
        ChatReq { playerid, text } ->
            messageValue "req" "chat" [ ("playerid", playerid)
                                      , ("text", text)
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

restoreStateEncoder : RestoreState -> Value
restoreStateEncoder state =
    JE.list [JE.string state.board
            , JE.list <| List.map JE.string state.players
            , JE.int state.resolver
            ]

encodeRestoreState : RestoreState -> String
encodeRestoreState state =
    JE.encode 0 <| restoreStateEncoder state
