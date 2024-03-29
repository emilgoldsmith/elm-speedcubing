module VerifyExamples.Cube.AddAUFsToAlgorithm0 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Cube exposing (..)
import AUF
import Algorithm







spec0 : Test.Test
spec0 =
    Test.test "#addAUFsToAlgorithm: \n\n    addAUFsToAlgorithm\n        ( AUF.Halfway, AUF.Clockwise )\n        (Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise ])\n    --> Algorithm.fromTurnList\n    -->     [ Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise\n    -->     , Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise\n    -->     , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise\n    -->     ]" <|
        \() ->
            Expect.equal
                (
                addAUFsToAlgorithm
                    ( AUF.Halfway, AUF.Clockwise )
                    (Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise ])
                )
                (
                Algorithm.fromTurnList
                    [ Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
                    , Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise
                    , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
                    ]
                )