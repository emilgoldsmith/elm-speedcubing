module VerifyExamples.AUF.All0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import AUF exposing (..)
import PLL
import List.Nonempty







spec0 : Test.Test
spec0 =
    Test.test "#all: \n\n    -- They are all there!\n    List.Nonempty.length all\n    --> 4" <|
        \() ->
            Expect.equal
                (
                -- They are all there!
                List.Nonempty.length all
                )
                (
                4
                )