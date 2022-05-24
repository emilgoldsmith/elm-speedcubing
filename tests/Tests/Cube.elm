module Tests.Cube exposing (addAUFsToAlgorithmTests, algorithmResultsAreEquivalentIndependentOfFinalRotationTests, algorithmResultsAreEquivalentTests, applyAlgorithmTests, detectAUFsTests, makeAlgorithmMaintainOrientationTests, testHelperTests)

import AUF
import Algorithm exposing (Algorithm)
import Cube
import Cube.Advanced exposing (Color(..))
import Expect
import Expect.Extra
import Fuzz
import List.Nonempty
import List.Nonempty.Extra
import PLL
import Parser exposing ((|.), (|=))
import Test exposing (..)
import TestHelpers.Cube exposing (cubeFuzzer, solvedCubeRendering)
import Tests.AUF exposing (aufFuzzer)
import Tests.Algorithm exposing (algorithmFuzzer, rotationFuzzer, turnDirectionFuzzer, turnFuzzer, turnableFuzzer)


applyAlgorithmTests : Test
applyAlgorithmTests =
    describe "applyAlgorithm"
        [ fuzz2 cubeFuzzer algorithmFuzzer "Applying an algorithm followed by its inverse results in the identity" <|
            \cube alg ->
                cube
                    |> Cube.applyAlgorithm alg
                    |> Cube.applyAlgorithm (Algorithm.inverse alg)
                    |> Expect.equal cube
        , fuzz2 cubeFuzzer turnFuzzer "Applying a single move is not an identity operation" <|
            \cube turn ->
                cube
                    |> Cube.applyAlgorithm (Algorithm.fromTurnList << List.singleton <| turn)
                    |> Expect.notEqual cube
        , fuzz3 cubeFuzzer algorithmFuzzer algorithmFuzzer "is associative, so applying combined or separated algs to cube should result in same cube" <|
            \cube alg1 alg2 ->
                let
                    appliedTogether =
                        cube |> Cube.applyAlgorithm (Algorithm.append alg1 alg2)

                    appliedSeparately =
                        cube |> Cube.applyAlgorithm alg1 |> Cube.applyAlgorithm alg2
                in
                appliedTogether |> Expect.equal appliedSeparately
        , fuzz2 commutativePairsFuzzer cubeFuzzer "parallel turns are commutative" <|
            \( turn1, turn2 ) cube ->
                Cube.applyAlgorithm (Algorithm.fromTurnList [ turn1, turn2 ]) cube
                    |> Expect.equal (Cube.applyAlgorithm (Algorithm.fromTurnList [ turn2, turn1 ]) cube)
        , fuzz2 nonCommutativePairsFuzzer cubeFuzzer "non parallel turns are not commutative" <|
            \( turn1, turn2 ) cube ->
                Cube.applyAlgorithm (Algorithm.fromTurnList [ turn1, turn2 ]) cube
                    |> Expect.notEqual (Cube.applyAlgorithm (Algorithm.fromTurnList [ turn2, turn1 ]) cube)
        , fuzz3 cubeFuzzer turnableFuzzer turnDirectionFuzzer "Applying a quarter turn twice equals applying a double turn" <|
            \cube turnable direction ->
                let
                    quarterTurn =
                        Algorithm.Turn turnable Algorithm.OneQuarter direction

                    doubleTurn =
                        Algorithm.Turn turnable Algorithm.Halfway direction

                    afterTwoQuarterTurns =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ quarterTurn, quarterTurn ])

                    afterOneHalfway =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ doubleTurn ])
                in
                afterTwoQuarterTurns |> Expect.equal afterOneHalfway
        , fuzz3 cubeFuzzer turnableFuzzer turnDirectionFuzzer "Applying a quarter turn thrice equals applying a triple turn" <|
            \cube turnable direction ->
                let
                    quarterTurn =
                        Algorithm.Turn turnable Algorithm.OneQuarter direction

                    tripleTurn =
                        Algorithm.Turn turnable Algorithm.ThreeQuarters direction

                    afterThreeQuarterTurns =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ quarterTurn, quarterTurn, quarterTurn ])

                    afterOneTripleTurn =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ tripleTurn ])
                in
                afterThreeQuarterTurns |> Expect.equal afterOneTripleTurn
        , fuzz3 cubeFuzzer turnableFuzzer turnDirectionFuzzer "Applying a quarter turn four times equals doing nothing" <|
            \cube turnable direction ->
                let
                    quarterTurn =
                        Algorithm.Turn turnable Algorithm.OneQuarter direction

                    afterFourQuarterTurns =
                        cube |> Cube.applyAlgorithm (Algorithm.fromTurnList [ quarterTurn, quarterTurn, quarterTurn, quarterTurn ])
                in
                afterFourQuarterTurns |> Expect.equal cube
        , fuzz2 cubeFuzzer turnFuzzer "Applying a NUM (e.g double, triple) turn in one direction equals applying a (4 - NUM) turn in the opposite direction" <|
            \cube ((Algorithm.Turn turnable length direction) as turn) ->
                let
                    flipDirection dir =
                        case dir of
                            Algorithm.Clockwise ->
                                Algorithm.CounterClockwise

                            Algorithm.CounterClockwise ->
                                Algorithm.Clockwise

                    flipLength len =
                        case len of
                            Algorithm.OneQuarter ->
                                Algorithm.ThreeQuarters

                            Algorithm.Halfway ->
                                Algorithm.Halfway

                            Algorithm.ThreeQuarters ->
                                Algorithm.OneQuarter

                    turnAlg =
                        Algorithm.fromTurnList << List.singleton <| turn

                    oppositeDirectionEquivalent =
                        Algorithm.fromTurnList << List.singleton <| Algorithm.Turn turnable (flipLength length) (flipDirection direction)
                in
                cube |> Cube.applyAlgorithm turnAlg |> Expect.equal (Cube.applyAlgorithm oppositeDirectionEquivalent cube)
        , test "solved cube has correct colors" <|
            \_ ->
                Cube.solved
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings solvedCubeRendering
        , test "U performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ U ] [ FrontColor, LeftColor, BackColor, RightColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "D performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ D ] [ FrontColor, RightColor, BackColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "L performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ L ] [ UpColor, FrontColor, DownColor, BackColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "R performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ R ] [ UpColor, BackColor, DownColor, FrontColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "F performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ F ] [ UpColor, RightColor, DownColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "B performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ B ] [ UpColor, LeftColor, DownColor, RightColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "M performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.M Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ M ] [ UpColor, FrontColor, DownColor, BackColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "S performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.S Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ S ] [ UpColor, RightColor, DownColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "E performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.E Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ E ] [ FrontColor, RightColor, BackColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Uw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Uw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ U, E ] [ FrontColor, LeftColor, BackColor, RightColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Dw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Dw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ D, E ] [ FrontColor, RightColor, BackColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Lw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Lw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ L, M ] [ UpColor, FrontColor, DownColor, BackColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Rw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Rw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ R, M ] [ UpColor, BackColor, DownColor, FrontColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Fw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Fw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ F, S ] [ UpColor, RightColor, DownColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "Bw performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Bw Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ B, S ] [ UpColor, LeftColor, DownColor, RightColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "x performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.X Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ L, M, R ] [ UpColor, BackColor, DownColor, FrontColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "y performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Y Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ U, E, D ] [ FrontColor, LeftColor, BackColor, RightColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "z performs expected transformation" <|
            \_ ->
                let
                    alg =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise ]

                    expectedRendering =
                        cycleColorsFromSolvedCubeOnSlices [ F, S, B ] [ UpColor, RightColor, DownColor, LeftColor ]
                in
                Cube.solved
                    |> Cube.applyAlgorithm alg
                    |> Cube.Advanced.render
                    |> Expect.Extra.equalCubeRenderings expectedRendering
        , test "0-length algorithm is identity operation which can be useful for things like AUF modelling" <|
            \_ ->
                Cube.solved |> Cube.applyAlgorithm Algorithm.empty |> Expect.equal Cube.solved
        ]


makeAlgorithmMaintainOrientationTests : Test
makeAlgorithmMaintainOrientationTests =
    describe "makeAlgorithmMaintainOrientation"
        [ fuzz algorithmWithNoOrientationChanges "is an identity operation when no orientations involved" <|
            \algorithm ->
                Cube.makeAlgorithmMaintainOrientation algorithm
                    |> Expect.equal algorithm
        , fuzz Tests.Algorithm.algorithmFuzzer "makes any and all algorithms finish in the same orientation it started in" <|
            \algorithm ->
                Cube.applyAlgorithm
                    (Cube.makeAlgorithmMaintainOrientation algorithm)
                    Cube.solved
                    |> Cube.Advanced.render
                    |> Expect.all
                        (let
                            assertCentersMatch getter =
                                getter
                                    >> Expect.equal
                                        (getter <| Cube.Advanced.render Cube.solved)
                         in
                         [ assertCentersMatch .u
                         , assertCentersMatch .d
                         , assertCentersMatch .l
                         , assertCentersMatch .r
                         , assertCentersMatch .f
                         , assertCentersMatch .b
                         ]
                        )
        , fuzz Tests.Algorithm.algorithmFuzzer "algorithm output doesn't change anything but the orientation of the cube compared to original algorithm" <|
            \algorithm ->
                Cube.algorithmResultsAreEquivalentIndependentOfFinalRotation
                    algorithm
                    (Cube.makeAlgorithmMaintainOrientation algorithm)
                    |> Expect.true "should produce same result independent of orientation"
        ]


algorithmResultsAreEquivalentTests : Test
algorithmResultsAreEquivalentTests =
    describe "algorithmResultsAreEquivalent"
        [ fuzz commutativePairsFuzzer "commutative pairs are equivalent algorithms in any order" <|
            \( turn1, turn2 ) ->
                Cube.algorithmResultsAreEquivalent
                    (Algorithm.fromTurnList [ turn1, turn2 ])
                    (Algorithm.fromTurnList [ turn2, turn1 ])
                    |> Expect.true "Algorithms should produce the same result"
        , fuzz nonCommutativePairsFuzzer "non commutative pairs are not equivalent algorithms in any order" <|
            \( turn1, turn2 ) ->
                Cube.algorithmResultsAreEquivalent
                    (Algorithm.fromTurnList [ turn1, turn2 ])
                    (Algorithm.fromTurnList [ turn2, turn1 ])
                    |> Expect.false "Algorithms should not produce the same result"
        ]


algorithmResultsAreEquivalentIndependentOfFinalRotationTests : Test
algorithmResultsAreEquivalentIndependentOfFinalRotationTests =
    describe "algorithmResultsAreEquivalentIndependentOfFinalRotation"
        [ fuzz3 commutativePairsFuzzer rotationFuzzer rotationFuzzer "commutative pairs are equivalent algorithms in any order also independent of final rotations applied" <|
            \( turn1, turn2 ) rotation1 rotation2 ->
                Cube.algorithmResultsAreEquivalentIndependentOfFinalRotation
                    (Algorithm.fromTurnList [ turn1, turn2 ] |> Algorithm.reverseAppend rotation1)
                    (Algorithm.fromTurnList [ turn2, turn1 ] |> Algorithm.reverseAppend rotation2)
                    |> Expect.true "Algorithms should produce the same result"
        , fuzz nonCommutativePairsExcludingRotationsFuzzer "non commutative pairs are not equivalent algorithms in any order" <|
            \( turn1, turn2 ) ->
                Cube.algorithmResultsAreEquivalentIndependentOfFinalRotation
                    (Algorithm.fromTurnList [ turn1, turn2 ])
                    (Algorithm.fromTurnList [ turn2, turn1 ])
                    |> Expect.false "Algorithms should not produce the same result"
        ]


addAUFsToAlgorithmTests : Test
addAUFsToAlgorithmTests =
    describe "addAUFsToAlgorithm"
        [ fuzz2 Tests.Algorithm.algorithmFuzzer (Fuzz.tuple ( aufFuzzer, aufFuzzer )) "adds the aufs on each side of the algorithm simply when algorithm maintains orientation" <|
            \possiblyOrientingAlgorithm (( preAUF, postAUF ) as aufs) ->
                let
                    nonOrientingAlgorithm =
                        Cube.makeAlgorithmMaintainOrientation possiblyOrientingAlgorithm
                in
                Cube.addAUFsToAlgorithm aufs nonOrientingAlgorithm
                    |> Expect.equal
                        ((Algorithm.toTurnList << AUF.toAlgorithm) preAUF
                            ++ Algorithm.toTurnList nonOrientingAlgorithm
                            ++ (Algorithm.toTurnList << AUF.toAlgorithm) postAUF
                            |> Algorithm.fromTurnList
                        )
        , fuzz2 Tests.Algorithm.algorithmFuzzer (Fuzz.tuple ( aufFuzzer, aufFuzzer )) "always adds AUFs to the original U face independent of final rotation" <|
            \algorithm aufs ->
                let
                    expectedEquivalency =
                        algorithm
                            |> Cube.makeAlgorithmMaintainOrientation
                            |> Cube.addAUFsToAlgorithm aufs
                in
                Cube.addAUFsToAlgorithm aufs algorithm
                    |> Cube.algorithmResultsAreEquivalentIndependentOfFinalRotation expectedEquivalency
                    |> Expect.true "should be equivalent to an algorithm that maintained orientation"
        ]


detectAUFsTests : Test
detectAUFsTests =
    describe "detectAUFs"
        [ fuzzWith
            { runs = 5 }
            (Fuzz.tuple3
                ( algorithmFuzzer
                , algorithmFuzzer
                , Fuzz.tuple ( aufFuzzer, aufFuzzer )
                )
            )
            "correctly detects aufs on the same algorithm with an identity sequence appended to it"
          <|
            \( algorithm, algorithmForIdentity, aufs ) ->
                let
                    identitySequence =
                        Algorithm.append algorithmForIdentity (Algorithm.inverse algorithmForIdentity)

                    algorithmToMatch =
                        Algorithm.append algorithm identitySequence
                            |> Cube.addAUFsToAlgorithm aufs

                    maybeDetectedAUFs =
                        Cube.detectAUFs { toMatchTo = algorithmToMatch, toDetectFor = algorithm }

                    resultingAlgorithm =
                        maybeDetectedAUFs
                            |> Maybe.map
                                (\detectedAUFs ->
                                    algorithm
                                        |> Cube.addAUFsToAlgorithm detectedAUFs
                                )
                in
                resultingAlgorithm
                    |> Maybe.map (Cube.algorithmResultsAreEquivalentIndependentOfFinalRotation algorithmToMatch)
                    |> Maybe.map (Expect.true "The algorithm built using detect should be equivalent to the one we're matching")
                    |> Maybe.withDefault (Expect.fail "No possible AUFs detected when some should have been found")
        , fuzzWith
            { runs = 5 }
            algorithmFuzzer
            "no matches are found for algorithms that are not equivalent no matter the AUF between them"
          <|
            \algorithm ->
                let
                    definitelyNotMatchingAlgorithm =
                        Algorithm.append
                            algorithm
                            (Algorithm.fromTurnList
                                -- An slice turn cannot be fixed by any AUF ever
                                [ Algorithm.Turn Algorithm.E Algorithm.Halfway Algorithm.Clockwise
                                ]
                            )
                in
                Cube.detectAUFs { toMatchTo = definitelyNotMatchingAlgorithm, toDetectFor = algorithm }
                    |> Expect.equal Nothing
        , test "correctly detects AUF for an algorithm ending in a non-standard orientation" <|
            \_ ->
                Algorithm.fromString "(x) R' U R' D2 R U' R' D2 R2"
                    |> Result.map
                        (\algorithm ->
                            Cube.detectAUFs
                                { toMatchTo = Cube.addAUFsToAlgorithm ( AUF.Halfway, AUF.Clockwise ) <| PLL.getAlgorithm PLL.referenceAlgorithms PLL.Aa
                                , toDetectFor = algorithm
                                }
                        )
                    |> Expect.equal (Ok (Just ( AUF.Halfway, AUF.Clockwise )))
        ]


testHelperTests : Test
testHelperTests =
    describe "test helper tests"
        [ describe "parallel turns"
            [ test "up or down group is disjoint with front or back group" <|
                \_ ->
                    listsDisjoint upOrDownParallelGroup frontOrBackParallelGroup
                        |> Expect.true "Expected groups to be disjoint"
            , test "up or down group is disjoint with left or right group" <|
                \_ ->
                    listsDisjoint upOrDownParallelGroup leftOrRightParallelGroup
                        |> Expect.true "Expected groups to be disjoint"
            , test "front or back group is disjoint with left or right group" <|
                \_ ->
                    listsDisjoint frontOrBackParallelGroup leftOrRightParallelGroup
                        |> Expect.true "Expected groups to be disjoint"
            , test "three parallel groups have same length as all turns" <|
                \_ ->
                    [ upOrDownParallelGroup, frontOrBackParallelGroup, leftOrRightParallelGroup ]
                        |> List.map List.Nonempty.length
                        |> List.sum
                        |> Expect.equal (List.Nonempty.length Algorithm.allTurns)
            , test "commutativePairs and nonCommutativePairs are disjoint" <|
                \_ ->
                    List.filter (\commutative -> List.member commutative nonCommutativePairs) commutativePairs
                        |> Expect.equal []
            , test "commutativePairs + nonCommutativePairs have same length as amount of pairs of turns" <|
                \_ ->
                    let
                        numTurns =
                            List.Nonempty.length Algorithm.allTurns

                        -- Every turn is matched with all turns that haven't been matched with yet
                        -- to avoid (a, b) (b, a) duplicates. This gives us an arithmetic sequence
                        -- of numTurns + (numTurns - 1) + ... + 1 which can be calculated with
                        -- the below formula, where we can safely assume numTurns is even (and
                        -- otherwise the test should fail anyway!)
                        numUniquePairs =
                            (numTurns + 1) * (numTurns // 2)
                    in
                    [ commutativePairs, nonCommutativePairs ]
                        |> List.map List.length
                        |> List.sum
                        |> Expect.equal numUniquePairs
            ]
        ]


algorithmWithNoOrientationChanges : Fuzz.Fuzzer Algorithm
algorithmWithNoOrientationChanges =
    Fuzz.map
        (Algorithm.toTurnList
            >> List.filter (not << isTurnThatMovesCenters)
            >> Algorithm.fromTurnList
        )
        Tests.Algorithm.algorithmFuzzer


listsDisjoint : List.Nonempty.Nonempty a -> List.Nonempty.Nonempty a -> Bool
listsDisjoint a b =
    List.filter (\aa -> List.Nonempty.member aa b) (List.Nonempty.toList a) == []


commutativePairsFuzzer : Fuzz.Fuzzer ( Algorithm.Turn, Algorithm.Turn )
commutativePairsFuzzer =
    commutativePairs
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


nonCommutativePairsFuzzer : Fuzz.Fuzzer ( Algorithm.Turn, Algorithm.Turn )
nonCommutativePairsFuzzer =
    nonCommutativePairs
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


nonCommutativePairsExcludingRotationsFuzzer : Fuzz.Fuzzer ( Algorithm.Turn, Algorithm.Turn )
nonCommutativePairsExcludingRotationsFuzzer =
    nonCommutativePairsExcludingRotations
        |> List.map Fuzz.constant
        |> Fuzz.oneOf


nonCommutativePairs : List ( Algorithm.Turn, Algorithm.Turn )
nonCommutativePairs =
    uniqueCartesianProductWithSelf Algorithm.allTurns
        |> List.Nonempty.toList
        |> List.filter (\anyPair -> not <| List.member anyPair commutativePairs)


nonCommutativePairsExcludingRotations : List ( Algorithm.Turn, Algorithm.Turn )
nonCommutativePairsExcludingRotations =
    List.filter
        (\( Algorithm.Turn turnable1 _ _, Algorithm.Turn turnable2 _ _ ) ->
            not (isWholeRotation turnable1) && not (isWholeRotation turnable2)
        )
        nonCommutativePairs


commutativePairs : List ( Algorithm.Turn, Algorithm.Turn )
commutativePairs =
    List.concatMap (uniqueCartesianProductWithSelf >> List.Nonempty.toList)
        [ upOrDownParallelGroup, frontOrBackParallelGroup, leftOrRightParallelGroup ]
        ++ List.Nonempty.toList nonParallelCommutativePairs


uniqueCartesianProductWithSelf : List.Nonempty.Nonempty Algorithm.Turn -> List.Nonempty.Nonempty ( Algorithm.Turn, Algorithm.Turn )
uniqueCartesianProductWithSelf group =
    List.Nonempty.Extra.lift2 Tuple.pair
        group
        group
        |> List.Nonempty.map
            (\( a, b ) ->
                if compareTurns a b == GT then
                    ( b, a )

                else
                    ( a, b )
            )
        |> List.Nonempty.sortWith
            (\( aa, ab ) ( ba, bb ) ->
                if compareTurns aa ba /= EQ then
                    compareTurns aa ba

                else
                    compareTurns ab bb
            )
        |> List.Nonempty.uniq


{-| There are some moves that are commutative despite not being parallel to each other
-}
nonParallelCommutativePairs : List.Nonempty.Nonempty ( Algorithm.Turn, Algorithm.Turn )
nonParallelCommutativePairs =
    let
        sliceAndRotationTurnables =
            List.Nonempty.filter
                (\x -> isSlice x || isWholeRotation x)
                Algorithm.M
                Algorithm.allTurnables

        allDoubleSliceOrRotationTurns =
            List.Nonempty.Extra.lift3 Algorithm.Turn
                sliceAndRotationTurnables
                (List.Nonempty.singleton Algorithm.Halfway)
                Algorithm.allTurnDirections
    in
    uniqueCartesianProductWithSelf allDoubleSliceOrRotationTurns
        -- Remove the parallel ones
        |> List.Nonempty.filter
            (Tuple.mapBoth getParallelGroup getParallelGroup >> (\( a, b ) -> a /= b))
            ( Algorithm.Turn Algorithm.X Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.Y Algorithm.Halfway Algorithm.Clockwise
            )


isSlice : Algorithm.Turnable -> Bool
isSlice turnable =
    List.member turnable [ Algorithm.M, Algorithm.S, Algorithm.E ]


isWholeRotation : Algorithm.Turnable -> Bool
isWholeRotation turnable =
    List.member turnable [ Algorithm.X, Algorithm.Y, Algorithm.Z ]


compareTurns : Algorithm.Turn -> Algorithm.Turn -> Order
compareTurns (Algorithm.Turn turnableA lengthA directionA) (Algorithm.Turn turnableB lengthB directionB) =
    let
        turnable =
            compareTurnables turnableA turnableB

        length =
            compareTurnLengths lengthA lengthB

        direction =
            compareTurnDirections directionA directionB
    in
    if turnable /= EQ then
        turnable

    else if length /= EQ then
        length

    else
        direction


compareTurnables : Algorithm.Turnable -> Algorithm.Turnable -> Order
compareTurnables =
    compareByListOrder Algorithm.allTurnables


compareTurnLengths : Algorithm.TurnLength -> Algorithm.TurnLength -> Order
compareTurnLengths =
    compareByListOrder Algorithm.allTurnLengths


compareTurnDirections : Algorithm.TurnDirection -> Algorithm.TurnDirection -> Order
compareTurnDirections =
    compareByListOrder Algorithm.allTurnDirections


compareByListOrder : List.Nonempty.Nonempty a -> a -> a -> Order
compareByListOrder order a b =
    let
        orderedElements =
            List.Nonempty.filter (\x -> x == a || x == b) a order
    in
    if a == b then
        EQ

    else
        List.Nonempty.head orderedElements
            -- Unsafe function in the sense that we assume there are the two elements
            -- we expect, so weird stuff could happen with bad inputs
            |> (\x ->
                    if x == a then
                        LT

                    else
                        GT
               )


type ParallelGroup
    = UpOrDownGroup
    | FrontOrBackGroup
    | LeftOrRightGroup


upOrDownParallelGroup : List.Nonempty.Nonempty Algorithm.Turn
upOrDownParallelGroup =
    List.partition
        (getParallelGroup >> (==) UpOrDownGroup)
        (Algorithm.allTurns |> List.Nonempty.toList)
        |> Tuple.first
        |> List.Nonempty.fromList
        |> (Maybe.withDefault <|
                List.Nonempty.singleton (Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise)
           )


frontOrBackParallelGroup : List.Nonempty.Nonempty Algorithm.Turn
frontOrBackParallelGroup =
    List.partition
        (getParallelGroup >> (==) FrontOrBackGroup)
        (Algorithm.allTurns |> List.Nonempty.toList)
        |> Tuple.first
        |> List.Nonempty.fromList
        |> (Maybe.withDefault <|
                List.Nonempty.singleton (Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise)
           )


leftOrRightParallelGroup : List.Nonempty.Nonempty Algorithm.Turn
leftOrRightParallelGroup =
    List.partition
        (getParallelGroup >> (==) LeftOrRightGroup)
        (Algorithm.allTurns |> List.Nonempty.toList)
        |> Tuple.first
        |> List.Nonempty.fromList
        |> (Maybe.withDefault <|
                List.Nonempty.singleton (Algorithm.Turn Algorithm.L Algorithm.Halfway Algorithm.Clockwise)
           )


getParallelGroup : Algorithm.Turn -> ParallelGroup
getParallelGroup turn =
    case turn of
        Algorithm.Turn Algorithm.U _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.D _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.Uw _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.Dw _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.E _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.Y _ _ ->
            UpOrDownGroup

        Algorithm.Turn Algorithm.F _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.B _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.Fw _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.Bw _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.S _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.Z _ _ ->
            FrontOrBackGroup

        Algorithm.Turn Algorithm.L _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.R _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.Lw _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.Rw _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.M _ _ ->
            LeftOrRightGroup

        Algorithm.Turn Algorithm.X _ _ ->
            LeftOrRightGroup


type Slice
    = U
    | D
    | L
    | R
    | F
    | B
    | M
    | S
    | E


cycleColorsFromSolvedCubeOnSlices : List Slice -> List Color -> Cube.Advanced.Rendering
cycleColorsFromSolvedCubeOnSlices slices colors =
    List.foldl
        (\slice rendering ->
            case slice of
                U ->
                    { rendering
                        | ufl = cycleCubieRendering colors rendering.ufl
                        , uf = cycleCubieRendering colors rendering.uf
                        , ufr = cycleCubieRendering colors rendering.ufr
                        , ur = cycleCubieRendering colors rendering.ur
                        , ubr = cycleCubieRendering colors rendering.ubr
                        , ub = cycleCubieRendering colors rendering.ub
                        , ubl = cycleCubieRendering colors rendering.ubl
                        , ul = cycleCubieRendering colors rendering.ul
                    }

                D ->
                    { rendering
                        | dfl = cycleCubieRendering colors rendering.dfl
                        , df = cycleCubieRendering colors rendering.df
                        , dfr = cycleCubieRendering colors rendering.dfr
                        , dr = cycleCubieRendering colors rendering.dr
                        , dbr = cycleCubieRendering colors rendering.dbr
                        , db = cycleCubieRendering colors rendering.db
                        , dbl = cycleCubieRendering colors rendering.dbl
                        , dl = cycleCubieRendering colors rendering.dl
                    }

                L ->
                    { rendering
                        | ufl = cycleCubieRendering colors rendering.ufl
                        , ul = cycleCubieRendering colors rendering.ul
                        , ubl = cycleCubieRendering colors rendering.ubl
                        , bl = cycleCubieRendering colors rendering.bl
                        , dbl = cycleCubieRendering colors rendering.dbl
                        , dl = cycleCubieRendering colors rendering.dl
                        , dfl = cycleCubieRendering colors rendering.dfl
                        , fl = cycleCubieRendering colors rendering.fl
                    }

                R ->
                    { rendering
                        | ufr = cycleCubieRendering colors rendering.ufr
                        , ur = cycleCubieRendering colors rendering.ur
                        , ubr = cycleCubieRendering colors rendering.ubr
                        , br = cycleCubieRendering colors rendering.br
                        , dbr = cycleCubieRendering colors rendering.dbr
                        , dr = cycleCubieRendering colors rendering.dr
                        , dfr = cycleCubieRendering colors rendering.dfr
                        , fr = cycleCubieRendering colors rendering.fr
                    }

                F ->
                    { rendering
                        | ufr = cycleCubieRendering colors rendering.ufr
                        , uf = cycleCubieRendering colors rendering.uf
                        , ufl = cycleCubieRendering colors rendering.ufl
                        , fl = cycleCubieRendering colors rendering.fl
                        , dfl = cycleCubieRendering colors rendering.dfl
                        , df = cycleCubieRendering colors rendering.df
                        , dfr = cycleCubieRendering colors rendering.dfr
                        , fr = cycleCubieRendering colors rendering.fr
                    }

                B ->
                    { rendering
                        | ubr = cycleCubieRendering colors rendering.ubr
                        , ub = cycleCubieRendering colors rendering.ub
                        , ubl = cycleCubieRendering colors rendering.ubl
                        , bl = cycleCubieRendering colors rendering.bl
                        , dbl = cycleCubieRendering colors rendering.dbl
                        , db = cycleCubieRendering colors rendering.db
                        , dbr = cycleCubieRendering colors rendering.dbr
                        , br = cycleCubieRendering colors rendering.br
                    }

                M ->
                    { rendering
                        | uf = cycleCubieRendering colors rendering.uf
                        , u = cycleCubieRendering colors rendering.u
                        , ub = cycleCubieRendering colors rendering.ub
                        , b = cycleCubieRendering colors rendering.b
                        , db = cycleCubieRendering colors rendering.db
                        , d = cycleCubieRendering colors rendering.d
                        , df = cycleCubieRendering colors rendering.df
                        , f = cycleCubieRendering colors rendering.f
                    }

                S ->
                    { rendering
                        | u = cycleCubieRendering colors rendering.u
                        , ul = cycleCubieRendering colors rendering.ul
                        , l = cycleCubieRendering colors rendering.l
                        , dl = cycleCubieRendering colors rendering.dl
                        , d = cycleCubieRendering colors rendering.d
                        , dr = cycleCubieRendering colors rendering.dr
                        , r = cycleCubieRendering colors rendering.r
                        , ur = cycleCubieRendering colors rendering.ur
                    }

                E ->
                    { rendering
                        | f = cycleCubieRendering colors rendering.f
                        , fr = cycleCubieRendering colors rendering.fr
                        , r = cycleCubieRendering colors rendering.r
                        , br = cycleCubieRendering colors rendering.br
                        , b = cycleCubieRendering colors rendering.b
                        , bl = cycleCubieRendering colors rendering.bl
                        , l = cycleCubieRendering colors rendering.l
                        , fl = cycleCubieRendering colors rendering.fl
                    }
        )
        solvedCubeRendering
        slices


cycleCubieRendering : List Color -> Cube.Advanced.CubieRendering -> Cube.Advanced.CubieRendering
cycleCubieRendering colors cubie =
    { u = cycleColor colors cubie.u
    , d = cycleColor colors cubie.d
    , l = cycleColor colors cubie.l
    , r = cycleColor colors cubie.r
    , f = cycleColor colors cubie.f
    , b = cycleColor colors cubie.b
    }


cycleColor : List Color -> Color -> Color
cycleColor cycle color =
    let
        afterAndBefore =
            -- We go from the right as U -> F -> R means we need to do the following
            -- assignments R=F, F=U, U=R which goes from right to left
            List.foldr
                (\currentColor currentAfterAndBefore ->
                    if List.length currentAfterAndBefore == 1 then
                        -- The before color was found so this will be the after color
                        -- by definition
                        currentColor :: currentAfterAndBefore

                    else if color == currentColor then
                        -- We found the before color
                        [ currentColor ]

                    else
                        currentAfterAndBefore
                )
                []
                cycle
    in
    case afterAndBefore of
        after :: _ :: _ ->
            after

        [ _ ] ->
            -- If only one was found probably the match was the last
            -- element so then the after is the end of the list as we folded
            -- from the right
            cycle |> List.reverse |> List.head |> Maybe.withDefault color

        _ ->
            -- Any other colors we just leave unchanged
            color


isTurnThatMovesCenters : Algorithm.Turn -> Bool
isTurnThatMovesCenters (Algorithm.Turn turnable _ _) =
    case turnable of
        Algorithm.X ->
            True

        Algorithm.Y ->
            True

        Algorithm.Z ->
            True

        Algorithm.Rw ->
            True

        Algorithm.Lw ->
            True

        Algorithm.Uw ->
            True

        Algorithm.Dw ->
            True

        Algorithm.Fw ->
            True

        Algorithm.Bw ->
            True

        Algorithm.M ->
            True

        Algorithm.S ->
            True

        Algorithm.E ->
            True

        Algorithm.U ->
            False

        Algorithm.D ->
            False

        Algorithm.L ->
            False

        Algorithm.R ->
            False

        Algorithm.F ->
            False

        Algorithm.B ->
            False
