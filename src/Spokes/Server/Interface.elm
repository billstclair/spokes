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

import Spokes.Server.EncodeDecode exposing ( Message )

import Spokes.Types exposing ( Board, DisplayList, Move(..), RenderInfo
                             , emptyDisplayList )

import Spokes.Board exposing ( renderInfo, computeDisplayList )

import Array exposing ( Array )

type Phase
    = IdlePhase
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
      , wrapper : (Message, ServerInterface msg) -> msg
      , state : Maybe ServerState
      , sender : ServerInterface msg -> Message -> Cmd msg
      }

makeProxyServer : ((Message, ServerInterface msg) -> msg) -> ServerInterface msg
makeProxyServer wrapper =
    ServerInterface { server = ""
                    , wrapper = wrapper
                    , state = Nothing
                    , sender = proxySender
                    }

proxySender : ServerInterface msg -> Message -> Cmd msg
proxySender interface message =
    Cmd.none
