module VerifyExamples.PLL.ReferenceAlgorithms16 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec16 : Test.Test
spec16 =
    Test.test "#referenceAlgorithms: \n\n    Algorithm.fromString \"R' F R' B2 R F' R' B2 R2\"\n    --> Ok referenceAlgorithms.aa" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "R' F R' B2 R F' R' B2 R2"
                )
                (
                Ok referenceAlgorithms.aa
                )