module Tests.PLL exposing (getAlgorithmTests, getAllAUFEquivalencyClassesTests, getAllEquivalentAUFsTests, getUniqueTwoSidedRecognitionSpecificationTests, referenceAlgTests, solvedByTests)

import AUF exposing (AUF)
import Algorithm
import Cube
import Cube.Advanced exposing (Color(..))
import Expect
import Expect.Extra
import Fuzz
import List.Extra
import List.Nonempty
import List.Nonempty.Extra
import PLL exposing (PLL)
import Random
import Test exposing (..)
import TestHelpers.Cube exposing (plainCubie, solvedCubeRendering)
import Tests.AUF exposing (aufFuzzer)
import Tests.Algorithm exposing (rotationFuzzer)


referenceAlgTests : Test
referenceAlgTests =
    describe "referenceAlgs"
        [ test "H perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = BackColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = FrontColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                in
                PLL.referenceAlgorithms.h
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ua perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = LeftColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                in
                PLL.referenceAlgorithms.ua
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ub perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = FrontColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                in
                PLL.referenceAlgorithms.ub
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Z perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = BackColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = LeftColor } })
                in
                PLL.referenceAlgorithms.z
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Aa perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = LeftColor, r = BackColor } })
                in
                PLL.referenceAlgorithms.aa
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ab perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = RightColor, l = BackColor } })
                in
                PLL.referenceAlgorithms.ab
                    |> expectEqualDisregardingAUF expectedRendering
        , test "E perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = LeftColor, l = FrontColor } })
                            |> (\x -> { x | ufl = { plainCubie | u = UpColor, f = LeftColor, l = BackColor } })
                in
                PLL.referenceAlgorithms.e
                    |> expectEqualDisregardingAUF expectedRendering
        , test "F perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = FrontColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = BackColor } })
                in
                PLL.referenceAlgorithms.f
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ga perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = LeftColor, r = BackColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = BackColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = RightColor } })
                in
                PLL.referenceAlgorithms.ga
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Gb perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = RightColor, l = BackColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = FrontColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = BackColor } })
                in
                PLL.referenceAlgorithms.gb
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Gc perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = RightColor, l = BackColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = LeftColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = BackColor } })
                in
                PLL.referenceAlgorithms.gc
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Gd perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = LeftColor, r = BackColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = BackColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = RightColor } })
                in
                PLL.referenceAlgorithms.gd
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ja perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = BackColor } })
                in
                PLL.referenceAlgorithms.ja
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Jb perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                in
                PLL.referenceAlgorithms.jb
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Na perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = BackColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = FrontColor } })
                in
                PLL.referenceAlgorithms.na
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Nb perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                in
                PLL.referenceAlgorithms.nb
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Ra perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ub = { plainCubie | u = UpColor, b = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = BackColor } })
                in
                PLL.referenceAlgorithms.ra
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Rb perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = FrontColor } })
                in
                PLL.referenceAlgorithms.rb
                    |> expectEqualDisregardingAUF expectedRendering
        , test "T perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = RightColor, r = BackColor } })
                            |> (\x -> { x | ubr = { plainCubie | u = UpColor, b = RightColor, r = FrontColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = RightColor } })
                in
                PLL.referenceAlgorithms.t
                    |> expectEqualDisregardingAUF expectedRendering
        , test "V perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = LeftColor } })
                            |> (\x -> { x | ul = { plainCubie | u = UpColor, l = FrontColor } })
                in
                PLL.referenceAlgorithms.v
                    |> expectEqualDisregardingAUF expectedRendering
        , test "Y perm" <|
            \_ ->
                let
                    expectedRendering =
                        solvedCubeRendering
                            |> (\x -> { x | ufr = { plainCubie | u = UpColor, f = BackColor, r = LeftColor } })
                            |> (\x -> { x | ubl = { plainCubie | u = UpColor, b = FrontColor, l = RightColor } })
                            |> (\x -> { x | uf = { plainCubie | u = UpColor, f = RightColor } })
                            |> (\x -> { x | ur = { plainCubie | u = UpColor, r = FrontColor } })
                in
                PLL.referenceAlgorithms.y
                    |> expectEqualDisregardingAUF expectedRendering
        ]


getAlgorithmTests : Test
getAlgorithmTests =
    describe "getAlgorithm"
        [ test "gets the algorithm from the provided algorithm set" <|
            \_ ->
                let
                    referenceAlgorithms =
                        PLL.referenceAlgorithms

                    expectedAlgorithm =
                        Algorithm.fromTurnList [ Algorithm.Turn Algorithm.M Algorithm.OneQuarter Algorithm.CounterClockwise ]

                    algorithmSet =
                        { referenceAlgorithms | aa = expectedAlgorithm }
                in
                PLL.getAlgorithm algorithmSet PLL.Aa
                    |> Expect.equal expectedAlgorithm
        ]


solvedByTests : Test
solvedByTests =
    describe "solvedBy"
        [ test "obviously wrong case fails check" <|
            \_ ->
                PLL.solvedBy Algorithm.empty PLL.Aa
                    |> Expect.false "Aa PLL was deemed solved by an empty algorithm"
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, aufFuzzer ))
            rotationFuzzer
            pllFuzzer
            "pll is solved by its reference algorithm no matter what auf combination or rotation applied to it"
          <|
            \( preAUF, postAUF ) rotation pll ->
                let
                    referenceAlgorithm =
                        PLL.getAlgorithm PLL.referenceAlgorithms pll

                    withAUFsAndRotation =
                        Algorithm.append
                            (Cube.addAUFsToAlgorithm ( preAUF, postAUF ) referenceAlgorithm)
                            rotation
                in
                PLL.solvedBy
                    withAUFsAndRotation
                    pll
                    |> Expect.true
                        ("PLL "
                            ++ PLL.getLetters pll
                            ++ " was not solved by its reference algorithm with pre AUF "
                            ++ (if String.isEmpty <| AUF.toString preAUF then
                                    "none"

                                else
                                    AUF.toString preAUF
                               )
                            ++ ", post AUF "
                            ++ (if String.isEmpty <| AUF.toString postAUF then
                                    "none"

                                else
                                    AUF.toString postAUF
                               )
                            ++ " and rotation "
                            ++ (if rotation == Algorithm.empty then
                                    "none"

                                else
                                    Algorithm.toString rotation
                               )
                        )
        , test "first version of an H perm passes H perm" <|
            \_ ->
                Algorithm.fromString "F2 M2' F2 U' F2 M2' F2"
                    |> Result.map (\alg -> PLL.solvedBy alg PLL.H)
                    |> Expect.equal (Ok True)
        , test "second version of an H perm passes H perm" <|
            \_ ->
                Algorithm.fromString "S R U2 R2 U2 R2 U2 R S'"
                    |> Result.map (\alg -> PLL.solvedBy alg PLL.H)
                    |> Expect.equal (Ok True)
        , test "an Ra perm with a y rotation works" <|
            \_ ->
                -- Taken from http://algdb.net/puzzle/333/pll/ra
                Algorithm.fromString "y R U R' F' R U2 R' U2 R' F R U R U2 R'"
                    |> Result.map (\alg -> PLL.solvedBy alg PLL.Ra)
                    |> Expect.equal (Ok True)
        , test "an Aa perm with a wide move that leaves cube with x rotation works" <|
            \_ ->
                -- Taken from http://algdb.net/puzzle/333/pll/aa
                Algorithm.fromString "l' U R' D2 R U' R' D2 R2"
                    |> Result.map (\alg -> PLL.solvedBy alg PLL.Aa)
                    |> Expect.equal (Ok True)
        , test "an Aa perm with different final AUF and a wide move that leaves cube with x rotation works" <|
            \_ ->
                -- Taken from http://algdb.net/puzzle/333/pll/aa
                -- with an addition of B which corresponds to U after the wide move
                Algorithm.fromString "l' U R' D2 R U' R' D2 R2 B"
                    |> Result.map (\alg -> PLL.solvedBy alg PLL.Aa)
                    |> Expect.equal (Ok True)
        , test "an algorithm with a leading z rotation that isn't reversed in the end works" <|
            \_ ->
                Algorithm.fromString "z D' R2 D R2 U R' D' R U' R U R' D R U'"
                    |> Result.map (\alg -> PLL.solvedBy alg PLL.V)
                    |> Expect.equal (Ok True)
        ]


getAllEquivalentAUFsTests : Test
getAllEquivalentAUFsTests =
    describe "getAllEquivalentAUFs"
        [ fuzz3
            Tests.AUF.aufFuzzer
            pllFuzzer
            Tests.AUF.aufFuzzer
            "all returned auf pairs are equivalent to the given auf pair"
          <|
            \preAUF pll postAUF ->
                ( preAUF, pll, postAUF )
                    |> PLL.getAllEquivalentAUFs
                    |> List.Nonempty.foldl
                        (\equivalentPair currentResult ->
                            if currentResult /= Expect.pass then
                                currentResult

                            else
                                Cube.algorithmResultsAreEquivalent
                                    (Cube.addAUFsToAlgorithm
                                        equivalentPair
                                        (PLL.getAlgorithm PLL.referenceAlgorithms pll)
                                    )
                                    (Cube.addAUFsToAlgorithm
                                        ( preAUF, postAUF )
                                        (PLL.getAlgorithm PLL.referenceAlgorithms pll)
                                    )
                                    |> Expect.true
                                        ("Not equivalent to ("
                                            ++ AUF.toString (Tuple.first equivalentPair)
                                            ++ ", "
                                            ++ AUF.toString (Tuple.second equivalentPair)
                                            ++ ")"
                                        )
                        )
                        Expect.pass
        , fuzz3
            pllFuzzer
            (Fuzz.tuple ( Tests.AUF.aufFuzzer, Tests.AUF.aufFuzzer ))
            (Fuzz.tuple ( Tests.AUF.aufFuzzer, Tests.AUF.aufFuzzer ))
            "auf pairs are included in the equivalent AUFs exactly if they are equivalent"
          <|
            \pll firstPair secondPair ->
                let
                    areEquivalent =
                        Cube.algorithmResultsAreEquivalent
                            (Cube.addAUFsToAlgorithm
                                firstPair
                                (PLL.getAlgorithm PLL.referenceAlgorithms pll)
                            )
                            (Cube.addAUFsToAlgorithm
                                secondPair
                                (PLL.getAlgorithm PLL.referenceAlgorithms pll)
                            )

                    isIncludedInEquivalents =
                        ( Tuple.first firstPair, pll, Tuple.second firstPair )
                            |> PLL.getAllEquivalentAUFs
                            |> List.Nonempty.member secondPair
                in
                isIncludedInEquivalents
                    |> Expect.equal areEquivalent
        ]


getAllAUFEquivalencyClassesTests : Test
getAllAUFEquivalencyClassesTests =
    describe "getAllAUFEquivalencyClasses"
        [ fuzz pllFuzzer "always has exactly all 16 unique auf pairs represented" <|
            \pll ->
                pll
                    |> PLL.getAllAUFEquivalencyClasses
                    |> List.Nonempty.concat
                    |> List.Nonempty.uniq
                    |> List.Nonempty.length
                    |> Expect.equal 16
        , fuzz3
            aufFuzzer
            pllFuzzer
            aufFuzzer
            "any given auf pair is represented and in the correct equivalency class"
          <|
            \preAUF pll postAUF ->
                let
                    targetEquivalencyClass =
                        PLL.getAllEquivalentAUFs ( preAUF, pll, postAUF )
                in
                pll
                    |> PLL.getAllAUFEquivalencyClasses
                    |> List.Nonempty.any
                        (\equivalencyClass ->
                            targetEquivalencyClass
                                |> Expect.Extra.equalNonEmptyListMembers equivalencyClass
                                |> (==) Expect.pass
                        )
                    |> Expect.true "There were no equivalency classes found that match the expected class"
        ]


recognitionAngleFuzzer : Fuzz.Fuzzer PLL.RecognitionAngle
recognitionAngleFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant PLL.ufrRecognitionAngle
        , Fuzz.constant PLL.uflRecognitionAngle
        ]


getUniqueTwoSidedRecognitionSpecificationTests : Test
getUniqueTwoSidedRecognitionSpecificationTests =
    describe "getUniqueTwoSidedRecognitionSpecificationTests"
        [ fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "no patterns (except absent ones) mentioned that are not included in the patterns"
          <|
            \case_ recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        case_
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok spec ->
                        allMentionedPatternsListedInPatterns spec
                            |> Expect.true ("There was a pattern mentioned not included in patterns. The spec was: " ++ Debug.toString spec)
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "no patterns mentioned that are included in absent patterns"
          <|
            \case_ recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        case_
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok spec ->
                        noMentionedPatternsIncludedInAbsentPatterns spec
                            |> Expect.true ("There was a pattern mentioned that was also included in absent patterns. The spec was: " ++ Debug.toString spec)

        -- This one also ensures that it's internally coherent as otherwise
        -- it wouldn't describe the case correctly if for example a sticker
        -- is supposed to be two different colors
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "the spec matches the case"
          <|
            \case_ recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        case_
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok spec ->
                        let
                            stickers =
                                getRecognitionStickers
                                    algorithms
                                    recognitionAngle
                                    case_
                        in
                        verifySpecForStickers stickers spec
                            |> Expect.true
                                ("the spec didn't correctly describe the stickers. The spec was:\n"
                                    ++ Debug.toString spec
                                    ++ "\nThe stickers were:\n"
                                    ++ Debug.toString stickers
                                )
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "check that no other cases except for symmetric ones match this spec; that it's therefore uniquely determinable by this description"
          <|
            \( preAUF, pll ) recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        ( preAUF, pll )
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok spec ->
                        let
                            equivalentPreAUFs =
                                PLL.getAllEquivalentAUFs ( preAUF, pll, AUF.None )
                                    |> List.Nonempty.toList
                                    |> List.map (\( x, _ ) -> x)

                            allOtherCases =
                                List.Nonempty.Extra.lift2
                                    Tuple.pair
                                    AUF.all
                                    PLL.all
                                    |> List.Nonempty.filter (\( preAUF_, pll_ ) -> not <| pll == pll_ && List.member preAUF_ equivalentPreAUFs)
                                        ( preAUF, pll )
                        in
                        allOtherCases
                            |> List.Nonempty.toList
                            |> List.filter
                                (\otherCase ->
                                    verifySpecForStickers
                                        (getRecognitionStickers
                                            algorithms
                                            recognitionAngle
                                            otherCase
                                        )
                                        spec
                                )
                            |> Expect.equalLists []
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "postAUF recognition gives correct elements going to correct faces"
          <|
            \case_ recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        case_
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok { postAUFRecognition } ->
                        postAUFRecognition
                            |> List.Nonempty.toList
                            |> List.filter
                                (\{ elementsWithOriginalFace, finalFace } ->
                                    let
                                        expectedElementColor =
                                            Cube.Advanced.faceToColor finalFace

                                        -- This is under the assumption that it gets the colors on
                                        -- cases that don't have any postAUF so that the cube would
                                        -- be solved after algorithm application and the color
                                        -- should therefore be the target face
                                        stickerColors =
                                            getRecognitionStickers algorithms recognitionAngle case_

                                        elementColors =
                                            elementsWithOriginalFace
                                                |> List.Nonempty.map Tuple.first
                                                |> List.Nonempty.concatMap getElementStickers
                                                |> List.Nonempty.map (getStickerColor stickerColors)
                                    in
                                    elementColors
                                        -- We only want to keep the ones that don't fit for a good
                                        -- error message
                                        |> List.Nonempty.any (\elementColor -> elementColor /= expectedElementColor)
                                )
                            |> Expect.equalLists []
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "postAUF recognition never gives more than two options, and options are never equal"
          <|
            \case_ recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        case_
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok { postAUFRecognition } ->
                        postAUFRecognition
                            |> Expect.all
                                [ List.Nonempty.length >> Expect.atMost 2
                                , List.Nonempty.uniq
                                    >> List.Nonempty.toList
                                    >> Expect.equalLists (List.Nonempty.toList postAUFRecognition)
                                ]
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "postAUF recognition never gives top or bottom as final face"
          <|
            \case_ recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        case_
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok { postAUFRecognition } ->
                        postAUFRecognition
                            |> List.Nonempty.Extra.find
                                (\{ finalFace } ->
                                    case finalFace of
                                        Cube.Advanced.UpOrDown _ ->
                                            True

                                        _ ->
                                            False
                                )
                            |> Expect.equal Nothing
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "postAUF recognition never references the same sticker in two different ways"
          <|
            \case_ recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        case_
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok { postAUFRecognition } ->
                        let
                            allReferencedStickers =
                                postAUFRecognition
                                    |> List.Nonempty.concatMap
                                        (.elementsWithOriginalFace
                                            >> List.Nonempty.map Tuple.first
                                        )
                        in
                        List.Nonempty.uniq allReferencedStickers
                            |> Expect.equal allReferencedStickers
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            recognitionAngleFuzzer
            pllAlgorithmsFuzzer
            "postAUF recognition never includes any obviously inferior options"
          <|
            \case_ recognitionAngle algorithms ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        case_
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok { postAUFRecognition } ->
                        -- 1. Doesn't stay visible < visible < stays in place
                        -- 2. Pattern visibility
                        -- If both of them are at least greater or equal and at least one is
                        -- strictly greater it is an obviously inferior option
                        postAUFRecognition
                            |> List.Nonempty.toList
                            |> List.filter
                                (\filteredElement ->
                                    -- Keep it only if it is inferior for better error messages
                                    postAUFRecognition
                                        |> List.Nonempty.any
                                            (\comparisonElement ->
                                                (visibilityScore filteredElement
                                                    <= visibilityScore comparisonElement
                                                )
                                                    && (finalFaceScore recognitionAngle filteredElement
                                                            <= finalFaceScore recognitionAngle comparisonElement
                                                            && ((visibilityScore filteredElement
                                                                    /= visibilityScore comparisonElement
                                                                )
                                                                    || (finalFaceScore recognitionAngle filteredElement
                                                                            /= finalFaceScore recognitionAngle comparisonElement
                                                                       )
                                                               )
                                                       )
                                            )
                                )
                            |> Expect.equalLists []
        , fuzz3
            (Fuzz.tuple ( aufFuzzer, pllFuzzer ))
            (Fuzz.tuple ( recognitionAngleFuzzer, pllAlgorithmsFuzzer ))
            (Fuzz.tuple ( Fuzz.intRange 0 7, Fuzz.intRange 0 Random.maxInt ))
            "spec cannot have any part removed and still uniquely identify the case"
          <|
            \( preAUF, pll ) ( recognitionAngle, algorithms ) ( keyIndex, subIndex ) ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        algorithms
                        recognitionAngle
                        ( preAUF, pll )
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok spec ->
                        case removePartFromSpec keyIndex subIndex spec of
                            Nothing ->
                                Expect.pass

                            Just ( specWithPartRemoved, partRemovedString ) ->
                                if
                                    not
                                        (allMentionedPatternsListedInPatterns specWithPartRemoved
                                            && noMentionedPatternsIncludedInAbsentPatterns specWithPartRemoved
                                            && verifySpecForStickers
                                                (getRecognitionStickers
                                                    algorithms
                                                    recognitionAngle
                                                    ( preAUF, pll )
                                                )
                                                specWithPartRemoved
                                        )
                                then
                                    -- The spec was made invalid by removing a part so we don't want
                                    -- the test to fail over an invalid spec
                                    Expect.pass

                                else
                                    let
                                        equivalentPreAUFs =
                                            PLL.getAllEquivalentAUFs ( preAUF, pll, AUF.None )
                                                |> List.Nonempty.toList
                                                |> List.map (\( x, _ ) -> x)

                                        allOtherCases =
                                            List.Nonempty.Extra.lift2
                                                Tuple.pair
                                                AUF.all
                                                PLL.all
                                                |> List.Nonempty.filter (\( preAUF_, pll_ ) -> not <| pll == pll_ && List.member preAUF_ equivalentPreAUFs)
                                                    ( preAUF, pll )
                                    in
                                    allOtherCases
                                        |> List.Nonempty.toList
                                        |> List.Extra.find
                                            (\otherCase ->
                                                verifySpecForStickers
                                                    (getRecognitionStickers
                                                        algorithms
                                                        recognitionAngle
                                                        otherCase
                                                    )
                                                    specWithPartRemoved
                                            )
                                        |> Expect.notEqual Nothing
                                        |> Expect.onFail
                                            ("The spec still uniquely identified the case, even though the following part was removed: "
                                                ++ partRemovedString
                                                ++ ". The spec after removing was: "
                                                ++ Debug.toString specWithPartRemoved
                                            )
        ]


visibilityScore :
    { elementsWithOriginalFace :
        List.Nonempty.Nonempty
            ( PLL.RecognitionElement
            , Cube.Advanced.Face
            )
    , finalFace : Cube.Advanced.Face
    }
    -> Float
visibilityScore { elementsWithOriginalFace } =
    elementsWithOriginalFace
        |> List.Nonempty.toList
        |> List.map Tuple.first
        |> List.map visibilityOfRecognitionElement
        |> List.sum


visibilityOfRecognitionElement : PLL.RecognitionElement -> Float
visibilityOfRecognitionElement element =
    case element of
        PLL.Sticker _ ->
            1

        PLL.Pattern _ ->
            10


finalFaceScore :
    PLL.RecognitionAngle
    ->
        { elementsWithOriginalFace :
            List.Nonempty.Nonempty
                ( PLL.RecognitionElement
                , Cube.Advanced.Face
                )
        , finalFace : Cube.Advanced.Face
        }
    -> Float
finalFaceScore angle spec =
    let
        staysInPlace =
            spec.elementsWithOriginalFace
                |> List.Nonempty.map Tuple.second
                |> List.Nonempty.all ((==) spec.finalFace)

        staysInPlaceScore =
            2

        staysVisibleScore =
            1

        elseScore =
            0
    in
    if staysInPlace then
        staysInPlaceScore

    else
        case spec.finalFace of
            Cube.Advanced.FrontOrBack Cube.Advanced.F ->
                staysVisibleScore

            Cube.Advanced.LeftOrRight Cube.Advanced.L ->
                if angle == PLL.uflRecognitionAngle then
                    staysVisibleScore

                else
                    elseScore

            Cube.Advanced.LeftOrRight Cube.Advanced.R ->
                if angle == PLL.ufrRecognitionAngle then
                    staysVisibleScore

                else
                    elseScore

            _ ->
                elseScore


noMentionedPatternsIncludedInAbsentPatterns : PLL.RecognitionSpecification -> Bool
noMentionedPatternsIncludedInAbsentPatterns spec =
    let
        absentPatterns =
            spec.caseRecognition.absentPatterns
                |> Maybe.map List.Nonempty.toList
                |> Maybe.withDefault []

        { caseRecognition } =
            spec

        otherPatternsCaseRecognition =
            { caseRecognition | absentPatterns = Nothing }

        otherPatterns =
            extractAllPatterns { spec | caseRecognition = otherPatternsCaseRecognition }
    in
    List.all (\x -> not <| List.member x absentPatterns) otherPatterns


allMentionedPatternsListedInPatterns : PLL.RecognitionSpecification -> Bool
allMentionedPatternsListedInPatterns spec =
    let
        patterns =
            spec.caseRecognition.patterns
                |> Maybe.map List.Nonempty.toList
                |> Maybe.withDefault []

        { caseRecognition } =
            spec

        otherPatternsCaseRecognition =
            { caseRecognition | patterns = Nothing, absentPatterns = Nothing }

        otherPatterns =
            extractAllPatterns { spec | caseRecognition = otherPatternsCaseRecognition }
    in
    List.all (\x -> List.member x patterns) otherPatterns


removePartFromSpec : Int -> Int -> PLL.RecognitionSpecification -> Maybe ( PLL.RecognitionSpecification, String )
removePartFromSpec keyIndex subIndex ({ caseRecognition } as spec) =
    case modBy 9 keyIndex of
        0 ->
            caseRecognition.patterns
                |> Maybe.map
                    (\patterns ->
                        let
                            fixedSubIndex =
                                modBy (List.Nonempty.length patterns) subIndex

                            removedItemString =
                                List.Nonempty.Extra.getAt fixedSubIndex patterns
                                    |> Maybe.map Debug.toString
                                    |> Maybe.withDefault ""

                            updatedCaseRecognition =
                                { caseRecognition
                                    | patterns = List.Nonempty.Extra.removeAt fixedSubIndex patterns
                                }
                        in
                        ( { spec | caseRecognition = updatedCaseRecognition }
                        , removedItemString ++ " from patterns key"
                        )
                    )

        1 ->
            caseRecognition.absentPatterns
                |> Maybe.map
                    (\absentPatterns ->
                        let
                            fixedSubIndex =
                                modBy (List.Nonempty.length absentPatterns) subIndex

                            removedItemString =
                                List.Nonempty.Extra.getAt fixedSubIndex absentPatterns
                                    |> Maybe.map Debug.toString
                                    |> Maybe.withDefault ""

                            updatedCaseRecognition =
                                { caseRecognition
                                    | absentPatterns = List.Nonempty.Extra.removeAt fixedSubIndex absentPatterns
                                }
                        in
                        ( { spec | caseRecognition = updatedCaseRecognition }
                        , removedItemString ++ " from absentPatterns key"
                        )
                    )

        2 ->
            if List.isEmpty caseRecognition.oppositelyColored then
                Nothing

            else
                let
                    ( newOppositelyColored, removedItemString ) =
                        removeItemFromNonemptyListPair
                            subIndex
                            caseRecognition.oppositelyColored

                    updatedCaseRecognition =
                        { caseRecognition
                            | oppositelyColored = newOppositelyColored
                        }
                in
                Just
                    ( { spec | caseRecognition = updatedCaseRecognition }
                    , removedItemString ++ " from oppositelyColored key"
                    )

        3 ->
            if List.isEmpty caseRecognition.adjacentlyColored then
                Nothing

            else
                let
                    ( newAdjacentlyColored, removedItemString ) =
                        removeItemFromNonemptyListPair
                            subIndex
                            caseRecognition.adjacentlyColored

                    updatedCaseRecognition =
                        { caseRecognition
                            | adjacentlyColored = newAdjacentlyColored
                        }
                in
                Just
                    ( { spec | caseRecognition = updatedCaseRecognition }
                    , removedItemString ++ " from adjacentlyColored key"
                    )

        4 ->
            if List.isEmpty caseRecognition.identicallyColored then
                Nothing

            else
                let
                    ( newIdenticallyColored, removedItemString ) =
                        removeItemFromMinLength2List
                            subIndex
                            caseRecognition.identicallyColored

                    updatedCaseRecognition =
                        { caseRecognition
                            | identicallyColored = newIdenticallyColored
                        }
                in
                Just
                    ( { spec | caseRecognition = updatedCaseRecognition }
                    , removedItemString ++ " from identicallyColored key"
                    )

        5 ->
            if List.isEmpty caseRecognition.differentlyColored then
                Nothing

            else
                let
                    ( differentlyColored, removedItemString ) =
                        removeItemFromMinLength2List
                            subIndex
                            caseRecognition.differentlyColored

                    updatedCaseRecognition =
                        { caseRecognition
                            | differentlyColored = differentlyColored
                        }
                in
                Just
                    ( { spec | caseRecognition = updatedCaseRecognition }
                    , removedItemString ++ " from differentlyColored key"
                    )

        6 ->
            caseRecognition.noOtherStickersMatchThanThese
                |> Maybe.map
                    (\noOtherStickersMatchThanThese ->
                        let
                            fixedSubIndex =
                                modBy
                                    (List.Nonempty.length noOtherStickersMatchThanThese)
                                    subIndex

                            removedItemString =
                                List.Nonempty.Extra.getAt fixedSubIndex noOtherStickersMatchThanThese
                                    |> Maybe.map Debug.toString
                                    |> Maybe.withDefault ""

                            updatedCaseRecognition =
                                { caseRecognition
                                    | noOtherStickersMatchThanThese = List.Nonempty.Extra.removeAt fixedSubIndex noOtherStickersMatchThanThese
                                }
                        in
                        ( { spec | caseRecognition = updatedCaseRecognition }
                        , removedItemString ++ " from noOtherStickersMatchThanThese key"
                        )
                    )

        7 ->
            if spec.caseRecognition.noOtherBlocksPresent == False then
                -- Nothing left to remove
                Nothing

            else
                let
                    updatedCaseRecognition =
                        { caseRecognition
                            | noOtherBlocksPresent = False
                        }
                in
                Just
                    ( { spec | caseRecognition = updatedCaseRecognition }
                    , "noOtherBlocksPresent set to false"
                    )

        _ ->
            Just
                ( { spec
                    | caseRecognition =
                        { patterns = Nothing
                        , absentPatterns = Nothing
                        , oppositelyColored = []
                        , adjacentlyColored = []
                        , identicallyColored = []
                        , differentlyColored = []
                        , noOtherStickersMatchThanThese = Nothing
                        , noOtherBlocksPresent = False
                        }
                  }
                , "invalid keyIndex"
                )


removeItemFromMinLength2List :
    Int
    -> List ( a, a, List a )
    -> ( List ( a, a, List a ), String )
removeItemFromMinLength2List subIndex list =
    let
        length =
            list
                |> List.map (minLength2ToList >> List.length)
                |> List.sum

        targetSubIndex =
            modBy length subIndex
    in
    list
        |> List.map minLength2ToList
        |> List.foldl
            (\current ( accList, curSubIndex, removedItem ) ->
                if curSubIndex > targetSubIndex then
                    ( current :: accList, curSubIndex, removedItem )

                else if curSubIndex + List.length current > targetSubIndex then
                    ( List.Extra.removeAt
                        (targetSubIndex - curSubIndex)
                        current
                        :: accList
                    , curSubIndex + List.length current
                    , List.Extra.getAt
                        (targetSubIndex - curSubIndex)
                        current
                        |> Maybe.map Debug.toString
                        |> Maybe.withDefault ""
                    )

                else
                    ( current :: accList
                    , curSubIndex + List.length current
                    , removedItem
                    )
            )
            ( [], 0, "" )
        |> (\( a, _, b ) ->
                ( a
                    |> List.filterMap minLength2FromList
                , b
                )
           )


minLength2ToList : ( a, a, List a ) -> List a
minLength2ToList ( first, second, tail ) =
    first :: second :: tail


minLength2FromList : List a -> Maybe ( a, a, List a )
minLength2FromList list =
    case list of
        first :: second :: tail ->
            Just ( first, second, tail )

        _ ->
            Nothing


removeItemFromNonemptyListPair :
    Int
    -> List ( List.Nonempty.Nonempty a, List.Nonempty.Nonempty a )
    -> ( List ( List.Nonempty.Nonempty a, List.Nonempty.Nonempty a ), String )
removeItemFromNonemptyListPair subIndex list =
    let
        length =
            list
                |> List.map
                    (Tuple.mapBoth
                        List.Nonempty.length
                        List.Nonempty.length
                    )
                |> List.map (\( a, b ) -> a + b)
                |> List.sum

        targetSubIndex =
            modBy length subIndex
    in
    list
        |> List.foldl
            (\( a, b ) ( accList, curSubIndex, removedItem ) ->
                if curSubIndex > targetSubIndex then
                    ( ( a, b ) :: accList, curSubIndex, removedItem )

                else if curSubIndex + List.Nonempty.length a > targetSubIndex then
                    ( List.Nonempty.Extra.removeAt
                        (targetSubIndex - curSubIndex)
                        a
                        |> Maybe.map
                            (\newA ->
                                ( newA, b ) :: accList
                            )
                        |> Maybe.withDefault accList
                    , curSubIndex + List.Nonempty.length a
                    , List.Nonempty.Extra.getAt
                        (targetSubIndex - curSubIndex)
                        a
                        |> Maybe.map Debug.toString
                        |> Maybe.withDefault ""
                    )

                else if
                    curSubIndex
                        + List.Nonempty.length a
                        + List.Nonempty.length b
                        > targetSubIndex
                then
                    ( List.Nonempty.Extra.removeAt
                        (targetSubIndex - List.Nonempty.length a - curSubIndex)
                        b
                        |> Maybe.map
                            (\newB ->
                                ( a, newB ) :: accList
                            )
                        |> Maybe.withDefault accList
                    , curSubIndex + List.Nonempty.length a + List.Nonempty.length b
                    , List.Nonempty.Extra.getAt
                        (targetSubIndex - List.Nonempty.length a - curSubIndex)
                        b
                        |> Maybe.map Debug.toString
                        |> Maybe.withDefault ""
                    )

                else
                    ( ( a, b ) :: accList
                    , curSubIndex
                        + List.Nonempty.length a
                        + List.Nonempty.length b
                    , removedItem
                    )
            )
            ( [], 0, "" )
        |> (\( a, _, b ) -> ( a, b ))


pllAlgorithmsFuzzer : Fuzz.Fuzzer PLL.Algorithms
pllAlgorithmsFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant PLL.referenceAlgorithms
        , Fuzz.constant jpermsAlgorithms
        ]


jpermsAlgorithms : PLL.Algorithms
jpermsAlgorithms =
    { ua =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "M2 U M U2 M' U M2"
    , ub =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "M2 U' M U2 M' U' M2"
    , h =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "M2 U M2 U2 M2 U M2"
    , z =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "M U M2 U M2 U M U2 M2"
    , aa =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "x L2 D2 (L' U' L) D2 (L' U L')"
    , ab =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "x (L U' L) D2 (L' U L) D2 L2"
    , e =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "x' (L' U L D') (L' U' L D) (L' U' L D') (L' U L D)"
    , t =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U R' U') R' F R2 U' R' U' (R U R') F'"
    , f =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R' U' F' (R U R' U') R' F R2 U' R' U' (R U R') U R"
    , jb =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U R' F') (R U R' U') R' F R2 U' R'"
    , ja =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "x (R2 F R F') R U2 (r' U r) U2"
    , ra =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U' R' U') (R U R D) (R' U' R D') (R' U2 R')"
    , rb =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R2 F R (U R U' R') F' R U2 R' U2 R"
    , y =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "F (R U' R' U') (R U R') F' (R U R' U') (R' F R F')"
    , v =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R U' (R U R') D R D' R (U' D) R2 U R2 D' R2"
    , na =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U R' U) (R U R' F' R U R' U' R' F R2 U' R') (U2 R U' R')"
    , nb =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "r' D' F (r U' r') F' D (r2 U r' U') (r' F r F')"
    , ga =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R2 U R' U R' U' R U' R2 (U' D) (R' U R) D'"
    , gb =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R' U' R) (U D') R2 U R' U R U' R U' R2 D"
    , gc =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R2 U' R U' R U R' U R2 (U D') (R U' R') D"
    , gd =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U R') (U' D) R2 U' R U' R' U R' U R2 D'"
    }


type alias RecognitionStickerColors =
    { firstFromLeft : Cube.Advanced.Color
    , secondFromLeft : Cube.Advanced.Color
    , thirdFromLeft : Cube.Advanced.Color
    , firstFromRight : Cube.Advanced.Color
    , secondFromRight : Cube.Advanced.Color
    , thirdFromRight : Cube.Advanced.Color
    }


getRecognitionStickers : PLL.Algorithms -> PLL.RecognitionAngle -> ( AUF, PLL ) -> RecognitionStickerColors
getRecognitionStickers algorithms recognitionAngle ( preAUF, pll ) =
    let
        rotationToGetCorrectRecognitionAngle =
            if recognitionAngle == PLL.ufrRecognitionAngle then
                Algorithm.empty

            else
                -- uflRecognitionAngle
                Algorithm.fromTurnList
                    [ Algorithm.Turn Algorithm.Y Algorithm.OneQuarter Algorithm.CounterClockwise ]
    in
    Cube.solved
        |> Cube.applyAlgorithm
            (Algorithm.inverse <|
                Cube.makeAlgorithmMaintainOrientation <|
                    Algorithm.append (AUF.toAlgorithm preAUF) <|
                        PLL.getAlgorithm algorithms pll
            )
        |> Cube.applyAlgorithm rotationToGetCorrectRecognitionAngle
        |> Cube.Advanced.render
        |> (\rendering ->
                { firstFromLeft = rendering.ufl.f
                , secondFromLeft = rendering.uf.f
                , thirdFromLeft = rendering.ufr.f
                , thirdFromRight = rendering.ufr.r
                , secondFromRight = rendering.ur.r
                , firstFromRight = rendering.ubr.r
                }
           )


isPatternPresent : RecognitionStickerColors -> PLL.RecognitionPattern -> Bool
isPatternPresent colors pattern =
    case pattern of
        PLL.Bookends ->
            colors.firstFromLeft == colors.firstFromRight

        PLL.LeftHeadlights ->
            colors.firstFromLeft == colors.thirdFromLeft

        PLL.RightHeadlights ->
            colors.firstFromRight == colors.thirdFromRight

        PLL.LeftThreeBar ->
            (colors.firstFromLeft == colors.secondFromLeft)
                && (colors.secondFromLeft == colors.thirdFromLeft)

        PLL.RightThreeBar ->
            (colors.firstFromRight == colors.secondFromRight)
                && (colors.secondFromRight == colors.thirdFromRight)

        PLL.LeftInsideTwoBar ->
            colors.secondFromLeft == colors.thirdFromLeft

        PLL.RightInsideTwoBar ->
            colors.secondFromRight == colors.thirdFromRight

        PLL.LeftOutsideTwoBar ->
            colors.firstFromLeft == colors.secondFromLeft

        PLL.RightOutsideTwoBar ->
            colors.firstFromRight == colors.secondFromRight

        PLL.LeftFourChecker ->
            (colors.firstFromLeft == colors.thirdFromLeft)
                && (colors.secondFromLeft == colors.thirdFromRight)

        PLL.RightFourChecker ->
            (colors.firstFromRight == colors.thirdFromRight)
                && (colors.secondFromRight == colors.thirdFromLeft)

        PLL.InnerFourChecker ->
            (colors.secondFromLeft == colors.thirdFromRight)
                && (colors.thirdFromLeft == colors.secondFromRight)

        PLL.LeftFiveChecker ->
            (colors.firstFromLeft == colors.thirdFromLeft)
                && (colors.secondFromLeft == colors.thirdFromRight)
                && (colors.thirdFromLeft == colors.secondFromRight)

        PLL.RightFiveChecker ->
            (colors.firstFromRight == colors.thirdFromRight)
                && (colors.secondFromRight == colors.thirdFromLeft)
                && (colors.thirdFromRight == colors.secondFromLeft)

        PLL.SixChecker ->
            (colors.firstFromLeft == colors.thirdFromLeft)
                && (colors.secondFromLeft == colors.thirdFromRight)
                && (colors.thirdFromLeft == colors.secondFromRight)
                && (colors.thirdFromRight == colors.firstFromRight)


getElementStickers : PLL.RecognitionElement -> List.Nonempty.Nonempty PLL.Sticker
getElementStickers element =
    case element of
        PLL.Sticker sticker ->
            List.Nonempty.singleton sticker

        PLL.Pattern pattern ->
            getPatternStickers pattern


getPatternStickers : PLL.RecognitionPattern -> List.Nonempty.Nonempty PLL.Sticker
getPatternStickers pattern =
    case pattern of
        PLL.Bookends ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromLeft
                [ PLL.FirstStickerFromRight ]

        PLL.LeftHeadlights ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromLeft
                [ PLL.ThirdStickerFromLeft ]

        PLL.RightHeadlights ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromRight
                [ PLL.ThirdStickerFromRight ]

        PLL.LeftThreeBar ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromLeft
                [ PLL.SecondStickerFromLeft
                , PLL.ThirdStickerFromLeft
                ]

        PLL.RightThreeBar ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromRight
                [ PLL.SecondStickerFromRight
                , PLL.ThirdStickerFromRight
                ]

        PLL.LeftInsideTwoBar ->
            List.Nonempty.Nonempty
                PLL.SecondStickerFromLeft
                [ PLL.ThirdStickerFromLeft ]

        PLL.RightInsideTwoBar ->
            List.Nonempty.Nonempty
                PLL.SecondStickerFromRight
                [ PLL.ThirdStickerFromRight ]

        PLL.LeftOutsideTwoBar ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromLeft
                [ PLL.SecondStickerFromLeft ]

        PLL.RightOutsideTwoBar ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromRight
                [ PLL.SecondStickerFromRight ]

        PLL.LeftFourChecker ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromLeft
                [ PLL.SecondStickerFromLeft
                , PLL.ThirdStickerFromLeft
                , PLL.ThirdStickerFromRight
                ]

        PLL.RightFourChecker ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromRight
                [ PLL.SecondStickerFromRight
                , PLL.ThirdStickerFromRight
                , PLL.ThirdStickerFromLeft
                ]

        PLL.InnerFourChecker ->
            List.Nonempty.Nonempty
                PLL.SecondStickerFromLeft
                [ PLL.ThirdStickerFromLeft
                , PLL.ThirdStickerFromRight
                , PLL.SecondStickerFromRight
                ]

        PLL.LeftFiveChecker ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromLeft
                [ PLL.SecondStickerFromLeft
                , PLL.ThirdStickerFromLeft
                , PLL.ThirdStickerFromRight
                , PLL.SecondStickerFromRight
                ]

        PLL.RightFiveChecker ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromRight
                [ PLL.SecondStickerFromRight
                , PLL.ThirdStickerFromRight
                , PLL.ThirdStickerFromLeft
                , PLL.SecondStickerFromLeft
                ]

        PLL.SixChecker ->
            List.Nonempty.Nonempty
                PLL.FirstStickerFromLeft
                [ PLL.SecondStickerFromLeft
                , PLL.ThirdStickerFromLeft
                , PLL.ThirdStickerFromRight
                , PLL.SecondStickerFromRight
                , PLL.FirstStickerFromRight
                ]


getStickerColor : RecognitionStickerColors -> PLL.Sticker -> Cube.Advanced.Color
getStickerColor colors sticker =
    case sticker of
        PLL.FirstStickerFromLeft ->
            colors.firstFromLeft

        PLL.SecondStickerFromLeft ->
            colors.secondFromLeft

        PLL.ThirdStickerFromLeft ->
            colors.thirdFromLeft

        PLL.ThirdStickerFromRight ->
            colors.thirdFromRight

        PLL.SecondStickerFromRight ->
            colors.secondFromRight

        PLL.FirstStickerFromRight ->
            colors.firstFromRight


verifySpecForStickers : RecognitionStickerColors -> PLL.RecognitionSpecification -> Bool
verifySpecForStickers stickers spec =
    List.all
        identity
        [ case spec.caseRecognition.patterns of
            Nothing ->
                True

            Just patterns ->
                patterns
                    |> List.Nonempty.all (isPatternPresent stickers)
        , if not spec.caseRecognition.noOtherBlocksPresent then
            True

          else
            let
                stickersThatArePartOfBlocks =
                    [ ( PLL.FirstStickerFromLeft, stickers.firstFromLeft )
                    , ( PLL.SecondStickerFromLeft, stickers.secondFromLeft )
                    , ( PLL.ThirdStickerFromLeft, stickers.thirdFromLeft )
                    , ( PLL.ThirdStickerFromRight, stickers.thirdFromRight )
                    , ( PLL.SecondStickerFromRight, stickers.secondFromRight )
                    , ( PLL.FirstStickerFromRight, stickers.firstFromRight )
                    ]
                        -- Get all pairs of neighbours
                        |> List.foldl
                            (\cur { prev, acc } ->
                                case prev of
                                    Nothing ->
                                        { prev = Just cur, acc = acc }

                                    Just prev_ ->
                                        { prev = Just cur, acc = ( prev_, cur ) :: acc }
                            )
                            { prev = Nothing, acc = [] }
                        |> .acc
                        -- Keep all stickers that have an equal neighbour
                        |> List.concatMap
                            (\( a, b ) ->
                                if Tuple.second a == Tuple.second b then
                                    [ Tuple.first a, Tuple.first b ]

                                else
                                    []
                            )
                        |> List.Extra.unique

                stickersThatAreInPatternBlocks =
                    case spec.caseRecognition.patterns of
                        Nothing ->
                            []

                        Just patterns ->
                            patterns
                                |> List.Nonempty.toList
                                |> List.filter isBlockPattern
                                |> List.map getPatternStickers
                                |> List.concatMap List.Nonempty.toList
            in
            stickersThatArePartOfBlocks
                |> List.filter
                    (\x ->
                        not <| List.member x stickersThatAreInPatternBlocks
                    )
                |> List.isEmpty
        , case spec.caseRecognition.absentPatterns of
            Nothing ->
                True

            Just absentPatterns ->
                absentPatterns
                    |> List.Nonempty.all (not << isPatternPresent stickers)
        , spec.caseRecognition.oppositelyColored
            |> List.all
                (mapSameForBoth
                    (List.Nonempty.concatMap getElementStickers
                        >> List.Nonempty.map (getStickerColor stickers)
                        >> List.Nonempty.uniq
                    )
                    >> (\x ->
                            case x of
                                -- They have to each group be the same color all stickers
                                -- and for that color to be opposite to the one of the
                                -- other group
                                ( List.Nonempty.Nonempty firstColor [], List.Nonempty.Nonempty secondColor [] ) ->
                                    areOppositeColors firstColor secondColor

                                _ ->
                                    False
                       )
                )
        , spec.caseRecognition.adjacentlyColored
            |> List.all
                (mapSameForBoth
                    (List.Nonempty.concatMap getElementStickers
                        >> List.Nonempty.map (getStickerColor stickers)
                        >> List.Nonempty.uniq
                    )
                    >> (\x ->
                            case x of
                                -- They have to each group be the same color all stickers
                                -- and for that color to be adjacent to the one of the
                                -- other group
                                ( List.Nonempty.Nonempty firstColor [], List.Nonempty.Nonempty secondColor [] ) ->
                                    areAdjacentColors firstColor secondColor

                                _ ->
                                    False
                       )
                )
        , spec.caseRecognition.identicallyColored
            |> List.all
                (\( first, second, tail ) ->
                    List.Nonempty.Nonempty first (second :: tail)
                        |> List.Nonempty.concatMap getElementStickers
                        |> List.Nonempty.map (getStickerColor stickers)
                        |> List.Nonempty.uniq
                        |> (List.Nonempty.length >> (==) 1)
                )
        , spec.caseRecognition.differentlyColored
            |> List.all
                (\( first, second, tail ) ->
                    let
                        list =
                            List.Nonempty.Nonempty first (second :: tail)

                        expectedDistinctColors =
                            List.Nonempty.length list

                        allGroupsAreSameColored =
                            list
                                |> List.Nonempty.map
                                    (getElementStickers
                                        >> List.Nonempty.map (getStickerColor stickers)
                                        >> List.Nonempty.uniq
                                        >> List.Nonempty.length
                                    )
                                |> List.Nonempty.all ((==) 1)

                        numDistinctColors =
                            list
                                |> List.Nonempty.concatMap getElementStickers
                                |> List.Nonempty.map (getStickerColor stickers)
                                |> List.Nonempty.uniq
                                |> List.Nonempty.length
                    in
                    allGroupsAreSameColored && numDistinctColors == expectedDistinctColors
                )
        , case spec.caseRecognition.noOtherStickersMatchThanThese of
            Nothing ->
                True

            Just elements ->
                let
                    allPatternsVerified =
                        elements
                            |> List.Nonempty.all
                                (\element ->
                                    case element of
                                        PLL.Sticker _ ->
                                            True

                                        PLL.Pattern pattern ->
                                            isPatternPresent stickers pattern
                                )

                    -- The stickers we are okay having matches on
                    excludedStickers =
                        elements
                            |> List.Nonempty.concatMap getElementStickers

                    -- The colors no other stickers than the specified ones can match
                    excludedColors =
                        excludedStickers
                            |> List.Nonempty.map (getStickerColor stickers)
                            |> List.Nonempty.uniq

                    -- The stickers that can't have matches
                    includedStickers =
                        allStickers
                            |> List.Nonempty.toList
                            |> List.filter
                                (\x ->
                                    not <| List.Nonempty.member x excludedStickers
                                )

                    -- The colors included in those
                    includedColors =
                        includedStickers
                            |> List.map (getStickerColor stickers)
                            |> List.Extra.unique

                    noExcludedColorsAreMatched =
                        includedColors
                            |> List.all
                                (\x ->
                                    not <| List.Nonempty.member x excludedColors
                                )

                    allStickersHaveADistinctColor =
                        List.length includedStickers == List.length includedColors
                in
                allPatternsVerified && noExcludedColorsAreMatched && allStickersHaveADistinctColor
        ]


isBlockPattern : PLL.RecognitionPattern -> Bool
isBlockPattern pattern =
    case pattern of
        PLL.LeftHeadlights ->
            False

        PLL.RightHeadlights ->
            False

        PLL.Bookends ->
            False

        PLL.LeftFourChecker ->
            False

        PLL.RightFourChecker ->
            False

        PLL.InnerFourChecker ->
            False

        PLL.LeftFiveChecker ->
            False

        PLL.RightFiveChecker ->
            False

        PLL.SixChecker ->
            False

        PLL.LeftInsideTwoBar ->
            True

        PLL.RightInsideTwoBar ->
            True

        PLL.LeftOutsideTwoBar ->
            True

        PLL.RightOutsideTwoBar ->
            True

        PLL.LeftThreeBar ->
            True

        PLL.RightThreeBar ->
            True


mapSameForBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapSameForBoth f ( first, second ) =
    ( f first, f second )


allStickers : List.Nonempty.Nonempty PLL.Sticker
allStickers =
    List.Nonempty.Nonempty
        PLL.FirstStickerFromLeft
        [ PLL.SecondStickerFromLeft
        , PLL.ThirdStickerFromLeft
        , PLL.ThirdStickerFromRight
        , PLL.SecondStickerFromRight
        , PLL.FirstStickerFromRight
        ]


areOppositeColors : Cube.Advanced.Color -> Cube.Advanced.Color -> Bool
areOppositeColors a b =
    case ( a, b ) of
        ( Cube.Advanced.UpColor, Cube.Advanced.DownColor ) ->
            True

        ( Cube.Advanced.DownColor, Cube.Advanced.UpColor ) ->
            True

        ( Cube.Advanced.LeftColor, Cube.Advanced.RightColor ) ->
            True

        ( Cube.Advanced.RightColor, Cube.Advanced.LeftColor ) ->
            True

        ( Cube.Advanced.FrontColor, Cube.Advanced.BackColor ) ->
            True

        ( Cube.Advanced.BackColor, Cube.Advanced.FrontColor ) ->
            True

        _ ->
            False


areAdjacentColors : Cube.Advanced.Color -> Cube.Advanced.Color -> Bool
areAdjacentColors a b =
    not <| areOppositeColors a b


extractAllPatterns : PLL.RecognitionSpecification -> List PLL.RecognitionPattern
extractAllPatterns spec =
    List.Extra.unique <|
        extractPatternsFromMaybePatterns spec.caseRecognition.patterns
            ++ extractPatternsFromMaybePatterns spec.caseRecognition.absentPatterns
            ++ List.concatMap extractPatternsFromTuple spec.caseRecognition.oppositelyColored
            ++ List.concatMap extractPatternsFromTuple spec.caseRecognition.adjacentlyColored
            ++ List.concatMap extractPatternsFromMinLength2List spec.caseRecognition.identicallyColored
            ++ List.concatMap extractPatternsFromMinLength2List spec.caseRecognition.differentlyColored
            ++ extractPatternsFromMaybeElements spec.caseRecognition.noOtherStickersMatchThanThese


extractPatternsFromMaybePatterns : Maybe (List.Nonempty.Nonempty PLL.RecognitionPattern) -> List PLL.RecognitionPattern
extractPatternsFromMaybePatterns maybeList =
    maybeList
        |> Maybe.map List.Nonempty.toList
        |> Maybe.withDefault []


extractPatternsFromTuple : ( List.Nonempty.Nonempty PLL.RecognitionElement, List.Nonempty.Nonempty PLL.RecognitionElement ) -> List PLL.RecognitionPattern
extractPatternsFromTuple ( a, b ) =
    (List.Nonempty.toList a ++ List.Nonempty.toList b)
        |> List.filterMap getPattern


extractPatternsFromMinLength2List : ( PLL.RecognitionElement, PLL.RecognitionElement, List PLL.RecognitionElement ) -> List PLL.RecognitionPattern
extractPatternsFromMinLength2List ( first, second, tail ) =
    first
        :: second
        :: tail
        |> List.filterMap getPattern


extractPatternsFromMaybeElements : Maybe (List.Nonempty.Nonempty PLL.RecognitionElement) -> List PLL.RecognitionPattern
extractPatternsFromMaybeElements maybeList =
    maybeList
        |> Maybe.map List.Nonempty.toList
        |> Maybe.withDefault []
        |> List.filterMap getPattern


getPattern : PLL.RecognitionElement -> Maybe PLL.RecognitionPattern
getPattern element =
    case element of
        PLL.Pattern pattern ->
            Just pattern

        _ ->
            Nothing


expectEqualDisregardingAUF : Cube.Advanced.Rendering -> Algorithm.Algorithm -> Expect.Expectation
expectEqualDisregardingAUF expectedRendering alg =
    let
        aufAlgorithms =
            List.Nonempty.map AUF.toAlgorithm AUF.all

        candidates =
            List.Nonempty.Extra.lift2
                (\preAUF postAUF ->
                    Algorithm.append preAUF <|
                        Algorithm.append alg <|
                            postAUF
                )
                aufAlgorithms
                aufAlgorithms
                |> List.Nonempty.map ((\x -> Cube.applyAlgorithm x Cube.solved) >> Cube.Advanced.render)
    in
    List.filter ((==) expectedRendering) (List.Nonempty.toList candidates)
        |> List.length
        |> Expect.greaterThan 0
        |> Expect.onFail
            ("Algorithm with or without pre and post AUF did not produce the expected rendering. Closest diff was:"
                ++ "\n\n"
                ++ "(Actual != Expected)"
                ++ "\n\n"
                ++ getShortestDiff candidates expectedRendering
            )


getShortestDiff : List.Nonempty.Nonempty Cube.Advanced.Rendering -> Cube.Advanced.Rendering -> String
getShortestDiff candidates expected =
    let
        diffs =
            List.Nonempty.map (\x -> TestHelpers.Cube.compareCubeRenderings x expected) candidates
    in
    List.Nonempty.foldl1 getShorterString diffs


getShorterString : String -> String -> String
getShorterString a b =
    if String.length a < String.length b then
        a

    else
        b


pllFuzzer : Fuzz.Fuzzer PLL
pllFuzzer =
    PLL.all
        |> List.Nonempty.map Fuzz.constant
        |> List.Nonempty.toList
        |> Fuzz.oneOf
