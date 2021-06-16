module VerifyExamples.Algorithm.FromString0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Algorithm exposing (..)







spec0 : Test.Test
spec0 =
    Test.test "#fromString: \n\n    fromString \"U\"\n    --> Ok (fromTurnList [Turn U OneQuarter Clockwise])" <|
        \() ->
            Expect.equal
                (
                fromString "U"
                )
                (
                Ok (fromTurnList [Turn U OneQuarter Clockwise])
                )