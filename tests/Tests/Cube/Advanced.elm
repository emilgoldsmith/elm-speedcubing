module Tests.Cube.Advanced exposing (canBeSolvedBySingleUTurnTests)

import Algorithm
import Cube
import Cube.Advanced
import Expect
import Test exposing (..)
import Tests.Algorithm exposing (turnDirectionFuzzer, turnLengthFuzzer)


canBeSolvedBySingleUTurnTests : Test
canBeSolvedBySingleUTurnTests =
    describe "canBeSolvedBySingleUTurn"
        [ fuzz2
            turnLengthFuzzer
            turnDirectionFuzzer
            "all single u turns applied to a solved cube return true"
          <|
            \turnLength turnDirection ->
                Cube.solved
                    |> Cube.applyAlgorithm
                        (Algorithm.fromTurnList
                            [ Algorithm.Turn Algorithm.U turnLength turnDirection
                            ]
                        )
                    |> Cube.Advanced.canBeSolvedBySingleUTurn
                    |> Expect.true "it couldn't be solved by a single U turn"
        , test "returns false for a single non-U turn applied to a cube" <|
            \_ ->
                Cube.solved
                    |> Cube.applyAlgorithm
                        (Algorithm.fromTurnList
                            [ Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
                            ]
                        )
                    |> Cube.Advanced.canBeSolvedBySingleUTurn
                    |> Expect.false "an F turn was able to be solved by a single U turn"
        , test "returns false for several moves that don't cancel with each other" <|
            \_ ->
                Cube.solved
                    |> Cube.applyAlgorithm
                        (Algorithm.fromTurnList
                            [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
                            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
                            ]
                        )
                    |> Cube.Advanced.canBeSolvedBySingleUTurn
                    |> Expect.false "UR was able to be solved by a single U turn"
        ]
