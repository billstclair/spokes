module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List
import Dict
import Maybe exposing ( withDefault )

import Spokes.Server.EncodeDecode as ED exposing ( Message(..) )
import Spokes.Types as Types exposing ( Move(..), Color(..), MovedStone(..) )

log = Debug.log

enableLogging : Bool
enableLogging =
  False                         --change to True to log JSON input & output results

maybeLog : String -> a -> a
maybeLog label value =
  if enableLogging then
    log label value
  else
    value

all : Test
all =
    Test.concat <|
        List.concat
            [ (List.map protocolTest protocolData)
            ]

expectResult : Result String Message -> Result String Message -> Expectation
expectResult sb was =
    case (maybeLog "  result" was) of
        Err msg ->
            case sb of
                Err _ ->
                    Expect.true "You shouldn't ever see this." True
                Ok _ ->
                    Expect.false msg True
        Ok wasv ->
            case sb of
                Err _ ->
                    Expect.false "Expected an error but didn't get one." True
                Ok sbv ->
                    Expect.equal sbv wasv

protocolTest : Message -> Test
protocolTest message =
    test ("protocolTest \"" ++ (toString message) ++ "\"")
        (\_ ->
             let json = maybeLog "protocolJson" <| ED.encodeMessage message
             in
                 expectResult (Ok message) <| ED.decodeMessage json
        )

protocolData : List Message
protocolData =
    [ RawMessage "foo" "bar" [("bletch", "gronk"), ("1", "2")]
    , NewReq { players = 2 }
    , NewRsp { gameid = "asdf" }
    , JoinReq { gameid = "asdf", name = "bill" }
    , JoinRsp { gameid = "asdf", name = "bill", number = 1 }
    , PlacephaseRsp { gameid = "asdf", turn = 2, resolver = 2 }
    , PlaceReq { gameid = "asdf", placement = Placement White "C1", number = 1 }
    , PlaceReq { gameid = "asdf", placement = Placement Black "D1", number = 2 }
    , PlaceRsp { gameid = "asdf", number = 2 }
    , PlacedRsp { gameid = "asdf"
                , placements = [ Placement White "C1"
                               , Placement Black "D1"
                               ]
                }
    , ResolveReq { gameid = "asdf", resolution = Resolution MoveWhite "D1" "1" }
    , ResolveReq { gameid = "asdf", resolution = Resolution MoveBlack "D2" "2" }
    , ResolveReq { gameid = "asdf", resolution = Resolution MoveBlock "C3" "D3" }
    , ResolveRsp { gameid = "asdf", resolution = Resolution MoveBlock "C3" "D3" }
    , UndoReq { gameid = "asdf"
              , message = PlacedRsp
                          { gameid = "asdf"
                          , placements = [ Placement White "C1"
                                         , Placement Black "D1"
                                         ]
                          }
              }
    , UndoRsp { gameid = "asdf"
              , message = ResolveRsp
                          { gameid = "asdf"
                          , resolution = Resolution MoveBlock "C3" "D3"
                          }
              }
    , ErrorRsp { request = "foo", id = 1, text = "Malformed request." }
    , ChatReq { gameid = "asdf", text = "Hello, World!" }
    , ChatRsp { gameid = "asdf", text = "Hello, World!", number = 1 }
    ]
