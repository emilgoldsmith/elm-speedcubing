module VerifyExamples.Cube.Advanced.CornerLocations0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Cube.Advanced exposing (..)
import List.Nonempty







spec0 : Test.Test
spec0 =
    Test.test "#cornerLocations: \n\n    List.Nonempty.length cornerLocations\n    --> 8" <|
        \() ->
            Expect.equal
                (
                List.Nonempty.length cornerLocations
                )
                (
                8
                )