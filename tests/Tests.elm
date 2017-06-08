module Tests exposing (all)

import Test exposing (..)
import Expect exposing ( Expectation )
import List
import Dict
import Maybe exposing ( withDefault )

import Spokes.Server.EncodeDecode as ED
import Spokes.Types as Types exposing ( Move(..), Color(..), MovedStone(..)
                                      , Message(..), GameOverReason(..)
                                      )

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
    , NewReq { players = 2, name = "Fred" }
    , NewRsp { gameid = "asdf", playerid = "p1", players = 4, name = "John" }
    , JoinReq { gameid = "asdf", name = "bill" }
    , JoinRsp { gameid = "asdf", playerid = Just "p1", name = "bill", number = 1 }
    , PlaceReq { playerid = "p1", placement = Placement White "C1" }
    , PlaceReq { playerid = "p2", placement = Placement Black "D1" }
    , PlaceRsp { gameid = "asdf", number = 2 }
    , PlacedRsp { gameid = "asdf"
                , placements = [ Placement White "C1"
                               , Placement Black "D1"
                               ]
                }
    , ResolveReq { playerid = "p1", resolution = Resolution MoveWhite "D1" "1" }
    , ResolveReq { playerid = "p2", resolution = Resolution MoveBlack "D2" "2" }
    , ResolveReq { playerid = "p1", resolution = Resolution MoveBlock "C3" "D3" }
    , ResolveRsp { gameid = "asdf", resolution = Resolution MoveBlock "C3" "D3" }
    , ResignReq { playerid = "p1" }
    , ResignRsp { gameid = "asdf", number = 1 }
    , GameOverRsp { gameid = "asdf", reason = ResignationReason 2 }
    , GameOverRsp { gameid = "asdf", reason = UnresolvableReason }
    , GameOverRsp { gameid = "asdf", reason = HomeCircleFullReason 1 }
    , GameOverRsp { gameid = "asdf", reason = TimeoutReason }
    , GameOverRsp { gameid = "asdf", reason = UnknownReason "unknown" }
    , UndoReq { playerid = "p1"
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
    , ChatReq { playerid = "p1", text = "Hello, World!" }
    , ChatRsp { gameid = "asdf", text = "Hello, World!", number = 1 }
    ]
