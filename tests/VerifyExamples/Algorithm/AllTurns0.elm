module VerifyExamples.Algorithm.AllTurns0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Algorithm exposing (..)
import List.Nonempty







spec0 : Test.Test
spec0 =
    Test.test "#allTurns: \n\n    List.Nonempty.length allTurns\n    --> List.Nonempty.length allTurnables\n    -->     * List.Nonempty.length allTurnLengths\n    -->     * List.Nonempty.length allTurnDirections" <|
        \() ->
            Expect.equal
                (
                List.Nonempty.length allTurns
                )
                (
                List.Nonempty.length allTurnables
                    * List.Nonempty.length allTurnLengths
                    * List.Nonempty.length allTurnDirections
                )