module VerifyExamples.Cube.Advanced.CanBeSolvedBySingleUTurn1 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Cube.Advanced exposing (..)
import Algorithm







spec1 : Test.Test
spec1 =
    Test.test "#canBeSolvedBySingleUTurn: \n\n    Algorithm.fromString \"U\"\n        |> Result.map (\\alg -> canBeSolvedBySingleUTurn (applyAlgorithm alg solved))\n    --> Ok True" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "U"
                    |> Result.map (\alg -> canBeSolvedBySingleUTurn (applyAlgorithm alg solved))
                )
                (
                Ok True
                )