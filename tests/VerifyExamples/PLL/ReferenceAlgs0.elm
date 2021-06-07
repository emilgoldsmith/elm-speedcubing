module VerifyExamples.PLL.ReferenceAlgs0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import PLL exposing (..)
import Algorithm







spec0 : Test.Test
spec0 =
    Test.test "#referenceAlgs: \n\n    Algorithm.fromString \"F2 D R2 U R2 D' R' U' R F2 R' U R\"\n    --> Ok referenceAlgs.y" <|
        \() ->
            Expect.equal
                (
                Algorithm.fromString "F2 D R2 U R2 D' R' U' R F2 R' U R"
                )
                (
                Ok referenceAlgs.y
                )