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

module Spokes.Server.EncodeDecode exposing ( Message
                                           --, encode, decode, encoder, decoder
                                           )

import Spokes.Types exposing ( Color(..), Move(..) )

import Json.Decode as JD exposing ( Decoder )
import Json.Encode as JE exposing ( Value )

type Message
    = RawMessage String String (List (String, String))
    -- Basic game play
    | NewReq { players : Int }
    | NewRsp { gameid : String }
    | JoinReq { gameid : String, name : String }
    | JoinRsp { gameid : String, name : String, number : Int }
    | PlacephaseRsp { gameid : String, turn : Int, resolver : Int }
    | PlaceReq { gameid : String, placement : String }
    | PlaceRsp { gameid : String, number : Int }
    | PlacedRsp { gameid : String, placements : List String }
    | ResolveReq { gameid : String, move : Move }
    | ResolveRsp { gameid : String, move : Move }
    -- Errors
    | UndoReq { gameid : String, req: Message }
    | UndoRsp { gameid : String, req: Message }
    | ErrorRsp { req : Message, id : Int, text : String }
    -- Chat
    | ChatReq { gameid : String, text : String }
    | ChatRsp { gameid : String, text : String, number : Int }

rawMessageDecoder : Decoder Message
rawMessageDecoder =
    JD.map3 RawMessage
        (JD.index 0 JD.string)
        (JD.index 1 JD.string)
        (JD.index 2 (JD.keyValuePairs JD.string))

messageDecoder : Decoder Message
messageDecoder =
    JD.map parseRawMessage rawMessageDecoder

parseRawMessage : Message -> Message
parseRawMessage message =
    message
