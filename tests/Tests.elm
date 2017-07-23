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
import Spokes.Board as Board exposing (encodedStringToBoard, boardToEncodedString)

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
            , (List.map boardTest boardData)
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
    , NewReq { players = 2, name = "Fred", isPublic = False
             , restoreState = Nothing }
    , NewReq { players = 4, name = "Bob", isPublic = True
             , restoreState = Nothing
             }
    , NewReq { players = 4, name = "Bob", isPublic = False
             , restoreState = Just { board = "board"
                                   , players = [ "Bill", "Chris" ]
                                   , resolver = 1
                                   }
             }
    , NewRsp { gameid = "asdf", playerid = "p1", players = 4, name = "John"
             , restoreState = Nothing
             }
    , NewRsp { gameid = "asdf", playerid = "p2", players = 4, name = "George"
             , restoreState = Just { board = "board"
                                   , players = [ "Bill", "Chris" ]
                                   , resolver = 1
                                   }
             }
    , JoinReq { gameid = "asdf", name = "bill" }
    , JoinRsp { gameid = "asdf"
              , players = 2
              , name = "bill"
              , playerid = Just "p1"
              , number = 1
              , restoreState = Nothing
              }
    , JoinRsp { gameid = "asdf"
              , players = 2
              , name = "Chris"
              , playerid = Just "p2"
              , number = 2
              , restoreState = Just { board = "board"
                                    , players = [ "Bill", "Chris" ]
                                    , resolver = 1
                                    }
              }
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
    , ResponseCountReq { playerid = "p1", number = 20 }
    , ResponseCountRsp { gameid = "asdf"
                       , number = 21
                       , restoreState = { board = "board"
                                        , players = [ "Bill", "Chris" ]
                                        , resolver = 1
                                        }
                       , votedUnresolvable = [1, 3]
                       }
    , ResignReq { playerid = "p1" }
    , ResignRsp { gameid = "asdf", number = 1, placements = Nothing }
    , ResignRsp { gameid = "asdf", number = 1
                , placements = Just [ Placement White "C1"
                                    , Placement Black "D3"
                                    ]
                }
    , UnresolvableVoteReq { playerid = "p1", vote = True }
    , UnresolvableVoteRsp { gameid = "asdf", number = 1, vote = False }
    , RemoveStoneVoteReq { playerid = "p2"
                         , resolution = Resolution MoveWhite "D1" ""
                         , vote = True
                         }
    , RemoveStoneVoteRsp { gameid = "asdf"
                         , number = 2
                         , resolution = Resolution MoveWhite "D1" ""
                         , vote = True
                         }
    , GameOverRsp { gameid = "asdf", reason = ResignationReason 2 }
    , GameOverRsp { gameid = "asdf"
                  , reason = UnresolvableReason [ Placement White "C1"
                                                , Placement Black "D3"
                                                ]
                  }
    , GameOverRsp { gameid = "asdf"
                  , reason = UnresolvableReason [ Resolution MoveWhite "D1" "1" ]
                  }
    , GameOverRsp { gameid = "asdf"
                  , reason = UnresolvableVoteReason
                             [ Resolution MoveWhite "D1" "" ]
                  }
    , GameOverRsp { gameid = "asdf"
                  , reason = HomeCircleFullReason
                             1
                             [ Placement White "C2"
                             , Placement Black "D4"
                             ]
                  }
    , GameOverRsp { gameid = "asdf"
                  , reason = HomeCircleFullReason
                             1
                             [ Resolution MoveBlack "D2" "2" ]
                  }
    , GameOverRsp { gameid = "asdf", reason = TimeoutReason }
    , GameOverRsp { gameid = "asdf", reason = UnknownReason "unknown" }
    , GamesReq
    , GamesRsp { twoPlayer = [ { gameid = "1"
                               , players = 2
                               , playerNames = [ "Bill" ]
                               }
                             , { gameid = "2"
                               , players = 2
                               , playerNames = [ "Bob" ]
                               }
                             ]
               , fourPlayer = [ { gameid = "3"
                                , players = 4
                                , playerNames = [ "Joe", "Frank" ]
                                }
                              , { gameid = "4"
                                , players = 4
                                , playerNames = [ "Randy", "Mel", "Doug" ]
                                }
                              ]
               }
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

expectString : String -> String -> Expectation
expectString sb was =
    Expect.equal sb was

boardTest : String -> Test
boardTest encodedBoard =
    test ("boardTest \"" ++ encodedBoard ++ "\"")
        (\_ ->
             let board = encodedStringToBoard encodedBoard
             in
                 expectString encodedBoard <| boardToEncodedString board
        )

boardData : List String
boardData =
    [ "Z0L0"
    , "d0h1v01c01c01c01Q0"
    , "d0x10011001c01c01c01l01c01c01c01e01c01c01c01"
    ]
