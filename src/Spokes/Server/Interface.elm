----------------------------------------------------------------------
--
-- Interface.elm
-- JSON encoder and decoder for Spokes server wire protocol.
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Spokes.Server.Interface exposing ( .. )

import Spokes.Server.EncodeDecode exposing ( Message(..), encodeMessage )
import Spokes.Server.Error exposing ( ServerError(..), errnum )

import Spokes.Types exposing ( Board, DisplayList, Move(..), RenderInfo
                             , emptyDisplayList )

import Spokes.Board exposing ( renderInfo, computeDisplayList )


import Array exposing ( Array )
import Task

type Phase
    = JoinPhase
    | PlacementPhase
    | ResolutionPhase

type alias ServerState =
    { board : Board
    , renderInfo : RenderInfo
    , displayList : DisplayList
    , phase : Phase
    , players : Int
    , turn : Int
    , resolver : Int
    , placements : Array Move
    }

type ServerInterface msg
    = ServerInterface
      { server : String
      , wrapper : ServerInterface msg -> Message -> msg
      , state : Maybe ServerState
      , sender : ServerInterface msg -> Message -> Cmd msg
      }

makeProxyServer : (ServerInterface msg -> Message -> msg) -> ServerInterface msg
makeProxyServer wrapper =
    ServerInterface { server = ""
                    , wrapper = wrapper
                    , state = Nothing
                    , sender = proxySender
                    }

proxyCmd : ServerInterface msg -> Message -> Cmd msg
proxyCmd (ServerInterface interface) message =
    let task = Task.succeed message
        wrapper = interface.wrapper (ServerInterface interface)
    in
        Task.perform wrapper task

proxySender : ServerInterface msg -> Message -> Cmd msg
proxySender interface message =
    case message of
        {-
        -- Basic game play
        NewReq { players } ->
            messageValue "req" "new" [("players", toString players)]
        NewRsp { gameid } ->
            messageValue "rsp" "new" [("gameid", gameid)]
        JoinReq { gameid, name } ->
            messageValue "req" "join" [("gameid", gameid), ("name", name)]
        JoinRsp { gameid, name, number } ->
            messageValue "rsp" "join" [ ("gameid", gameid)
                                      , ("name", name)
                                      , ("number", toString number)
                                      ]
        PlacephaseRsp { gameid, turn, resolver } ->
            messageValue "rsp" "placephase" [ ("gameid", gameid)
                                            , ("turn", toString turn)
                                            , ("resolver", toString resolver)
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
        ChatReq { gameid, text } ->
            messageValue "req" "chat" [ ("gameid", gameid)
                                      , ("text", text)
                                      ]
        ChatRsp { gameid, text, number } ->
            messageValue "rsp" "chat" [ ("gameid", gameid)
                                      , ("text", text)
                                      , ("number", toString number)
                                      ]
         -}
        _ ->
            let err = ErrorRsp { request = encodeMessage message
                               , id = errnum IllegalRequestErr
                               , text = "Illegal Request"
                               }
            in
                proxyCmd interface err
