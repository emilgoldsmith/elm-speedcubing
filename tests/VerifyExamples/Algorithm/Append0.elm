module VerifyExamples.Algorithm.Append0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Algorithm exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#append: \n\n    Result.map2 append (fromString \"U\") (fromString \"B'\")\n    --> fromString \"UB'\"" <|
        \() ->
            Expect.equal
                (
                Result.map2 append (fromString "U") (fromString "B'")
                )
                (
                fromString "UB'"
                )