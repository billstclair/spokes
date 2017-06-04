----------------------------------------------------------------------
--
-- Error.elm
-- Server error numbers
-- Copyright (c) 2017 Bill St. Clair <billstclair@gmail.com>
-- Some rights reserved.
-- Distributed under the MIT License
-- See LICENSE.txt
--
----------------------------------------------------------------------

module Spokes.Server.Error exposing ( ServerError (..), errnum )

import List.Extra as LE

import Dict exposing ( Dict )

type ServerError
    = MalformedJsonErr
    | IllegalRequestErr
    | IllegalPlayerCountErr
    | WrongGameidErr
    | UnparsableJsonErr
    | WrongPlayerNumberErr

errnums : List (ServerError, Int)
errnums =
    [ (MalformedJsonErr, 1)
    , (IllegalRequestErr, 2)
    , (IllegalPlayerCountErr, 3)
    , (WrongGameidErr, 4)
    , (UnparsableJsonErr, 5)
    , (WrongPlayerNumberErr, 6)
    ]

errnum : ServerError -> Int
errnum err =
    case LE.find (\(e, _) -> e == err) errnums of
        Nothing ->
            0
        Just (_, res) ->
            res
