module VerifyExamples.Internal.Cube.Faces0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Internal.Cube exposing (..)
import List.Nonempty







spec0 : Test.Test
spec0 =
    Test.test "#faces: \n\n    List.Nonempty.length faces\n    --> 6" <|
        \() ->
            Expect.equal
                (
                List.Nonempty.length faces
                )
                (
                6
                )