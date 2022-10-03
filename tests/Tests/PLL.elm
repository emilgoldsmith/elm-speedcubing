module Tests.PLL exposing (getAlgorithmTests, getAllAUFEquivalencyClassesTests, getAllEquivalentAUFsTests, getUniqueTwoSidedRecognitionSpecificationTests, referenceAlgTests, solvedByTests)

import AUF exposing (AUF)
import Algorithm exposing (Algorithm)
import Array
import Cube
import Cube.Advanced exposing (Color(..))
import Dict exposing (Dict)
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



-- Fuzzers


recognitionAngleFuzzer : Fuzz.Fuzzer PLL.RecognitionAngle
recognitionAngleFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant PLL.ufrRecognitionAngle
        , Fuzz.constant PLL.uflRecognitionAngle
        ]


pllFuzzer : Fuzz.Fuzzer PLL
pllFuzzer =
    PLL.all
        |> List.Nonempty.map Fuzz.constant
        |> List.Nonempty.toList
        |> Fuzz.oneOf


pllWithCorrectAlgorithmFuzzer : Fuzz.Fuzzer ( PLL, Algorithm )
pllWithCorrectAlgorithmFuzzer =
    Fuzz.map2
        (\pll index ->
            ( pll
            , getAlgorithmForPLL index pll
            )
        )
        pllFuzzer
        (Fuzz.intRange 0 100000)


getAlgorithmForPLL : Int -> PLL -> Algorithm
getAlgorithmForPLL index pll =
    Dict.get (PLL.getLetters pll) pllAlgorithms
        |> Maybe.withDefault [ Algorithm.empty ]
        |> Array.fromList
        |> (\array -> Array.get (modBy (Array.length array) index) array)
        |> Maybe.withDefault Algorithm.empty



-- Tests


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


getUniqueTwoSidedRecognitionSpecificationTests : Test
getUniqueTwoSidedRecognitionSpecificationTests =
    describe "getUniqueTwoSidedRecognitionSpecificationTests"
        [ fuzz3
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "no patterns (except absent ones) mentioned that are not included in the patterns"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok spec ->
                        allMentionedPatternsListedInPatterns spec
                            |> Expect.true ("There was a pattern mentioned not included in patterns. The spec was: " ++ Debug.toString spec)
        , fuzz3
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "no patterns mentioned that are included in absent patterns"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
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
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "the spec matches the case"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok spec ->
                        let
                            stickers =
                                getRecognitionStickers
                                    { pllAlgorithmUsed = algorithm
                                    , pll = pll
                                    , preAUF = preAUF
                                    , recognitionAngle = recognitionAngle
                                    }
                        in
                        verifySpecForStickers stickers spec
                            |> Expect.true
                                ("the spec didn't correctly describe the stickers. The spec was:\n"
                                    ++ Debug.toString spec
                                    ++ "\nThe stickers were:\n"
                                    ++ Debug.toString stickers
                                )
        , fuzz3
            (Fuzz.tuple ( pllFuzzer, Fuzz.intRange 0 100000 ))
            aufFuzzer
            recognitionAngleFuzzer
            "check that no other cases except for symmetric ones match this spec; that it's therefore uniquely determinable by this description"
          <|
            \( pll, algorithmIndex ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = getAlgorithmForPLL algorithmIndex pll
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok spec ->
                        getOtherNonSymmetricMatchingCases
                            { pllAlgorithmUsed = getAlgorithmForPLL algorithmIndex pll
                            , pll = pll
                            , preAUF = preAUF
                            , recognitionAngle = recognitionAngle
                            }
                            algorithmIndex
                            spec
                            |> Expect.equalLists []
        , fuzz3
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "postAUF recognition gives correct elements going to correct faces"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
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
                                            getRecognitionStickers
                                                { pllAlgorithmUsed = algorithm
                                                , pll = pll
                                                , preAUF = preAUF
                                                , recognitionAngle = recognitionAngle
                                                }

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
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "postAUF recognition never gives more than two options, and options are never equal"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
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
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "postAUF recognition gives all the correct original faces"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok { postAUFRecognition } ->
                        postAUFRecognition
                            |> List.Nonempty.toList
                            |> List.concatMap
                                (\{ elementsWithOriginalFace } ->
                                    elementsWithOriginalFace
                                        |> List.Nonempty.toList
                                        |> List.filter
                                            (\( element, originalFace ) ->
                                                element
                                                    |> getElementStickers
                                                    |> List.Nonempty.any
                                                        (\sticker ->
                                                            let
                                                                ufrExpectedFace =
                                                                    case sticker of
                                                                        PLL.FirstStickerFromLeft ->
                                                                            Cube.Advanced.FrontOrBack Cube.Advanced.F

                                                                        PLL.SecondStickerFromLeft ->
                                                                            Cube.Advanced.FrontOrBack Cube.Advanced.F

                                                                        PLL.ThirdStickerFromLeft ->
                                                                            Cube.Advanced.FrontOrBack Cube.Advanced.F

                                                                        PLL.FirstStickerFromRight ->
                                                                            Cube.Advanced.LeftOrRight Cube.Advanced.R

                                                                        PLL.SecondStickerFromRight ->
                                                                            Cube.Advanced.LeftOrRight Cube.Advanced.R

                                                                        PLL.ThirdStickerFromRight ->
                                                                            Cube.Advanced.LeftOrRight Cube.Advanced.R

                                                                recognitionAngleExpectedFace =
                                                                    if recognitionAngle == PLL.ufrRecognitionAngle then
                                                                        ufrExpectedFace

                                                                    else
                                                                        case ufrExpectedFace of
                                                                            Cube.Advanced.FrontOrBack Cube.Advanced.F ->
                                                                                Cube.Advanced.LeftOrRight Cube.Advanced.L

                                                                            Cube.Advanced.LeftOrRight Cube.Advanced.R ->
                                                                                Cube.Advanced.FrontOrBack Cube.Advanced.F

                                                                            _ ->
                                                                                Cube.Advanced.UpOrDown Cube.Advanced.U
                                                            in
                                                            originalFace /= recognitionAngleExpectedFace
                                                        )
                                            )
                                )
                            |> Expect.equalLists []
        , fuzz3
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "postAUF recognition never gives top or bottom as final face"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
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
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "postAUF recognition never references the same sticker in two different ways"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
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
            pllWithCorrectAlgorithmFuzzer
            aufFuzzer
            recognitionAngleFuzzer
            "postAUF recognition never includes any obviously inferior options"
          <|
            \( pll, algorithm ) preAUF recognitionAngle ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = algorithm
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
                of
                    Err err ->
                        Expect.fail ("spec failed: " ++ Debug.toString err)

                    Ok { postAUFRecognition } ->
                        -- 1. Doesn't stay visible < visible < stays in place
                        -- 2. Pattern visibility
                        -- If both of them are at least greater or equal and at least one is
                        -- strictly greater it is an obviously inferior option
                        List.Nonempty.Extra.lift2
                            Tuple.pair
                            postAUFRecognition
                            postAUFRecognition
                            |> List.Nonempty.toList
                            |> List.filter
                                (\( a, b ) ->
                                    -- Keep it only if it is inferior for better error messages
                                    (visibilityScore a <= visibilityScore b)
                                        && (finalFaceScore recognitionAngle a
                                                <= finalFaceScore recognitionAngle b
                                           )
                                        && ((visibilityScore a /= visibilityScore b)
                                                || (finalFaceScore recognitionAngle a
                                                        /= finalFaceScore recognitionAngle b
                                                   )
                                           )
                                )
                            |> Expect.equalLists []
        , fuzz3
            (Fuzz.tuple (pllFuzzer, Fuzz.intRange 0 10000))
            (Fuzz.tuple ( aufFuzzer, recognitionAngleFuzzer ))
            (Fuzz.tuple ( removePartKeyIndexRange, Fuzz.intRange 0 Random.maxInt ))
            "spec cannot have any part removed and still uniquely identify the case"
          <|
            \( pll, algorithmIndex ) ( preAUF, recognitionAngle ) ( keyIndex, subIndex ) ->
                case
                    PLL.getUniqueTwoSidedRecognitionSpecification
                        { pllAlgorithmUsed = getAlgorithmForPLL algorithmIndex pll
                        , pll = pll
                        , preAUF = preAUF
                        , recognitionAngle = recognitionAngle
                        }
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
                                                    { pllAlgorithmUsed = getAlgorithmForPLL algorithmIndex pll
                                                    , pll = pll
                                                    , preAUF = preAUF
                                                    , recognitionAngle = recognitionAngle
                                                    }
                                                )
                                                specWithPartRemoved
                                        )
                                then
                                    -- The spec was made invalid by removing a part so we don't want
                                    -- the test to fail over an invalid spec
                                    Expect.pass

                                else
                                    getOtherNonSymmetricMatchingCases
                                        { pllAlgorithmUsed = getAlgorithmForPLL algorithmIndex pll
                                        , pll = pll
                                        , preAUF = preAUF
                                        , recognitionAngle = recognitionAngle
                                        }
                                        algorithmIndex
                                        specWithPartRemoved
                                        |> Expect.notEqual []
                                        |> Expect.onFail
                                            ("The spec still uniquely identified the case, even though the following part was removed: "
                                                ++ partRemovedString
                                                ++ ". The spec after removing was: "
                                                ++ Debug.toString specWithPartRemoved
                                            )
        ]



-- Checks for two sided recognition


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


getOtherNonSymmetricMatchingCases :
    { pllAlgorithmUsed : Algorithm, recognitionAngle : PLL.RecognitionAngle, preAUF : AUF, pll : PLL }
    -> Int
    -> PLL.RecognitionSpecification
    -> List ( AUF, PLL )
getOtherNonSymmetricMatchingCases params algorithmIndex spec =
    let
        equivalentPreAUFs =
            PLL.getAllEquivalentAUFs ( params.preAUF, params.pll, AUF.None )
                |> List.Nonempty.toList
                |> List.map Tuple.first

        allOtherCases =
            List.Nonempty.Extra.lift2
                Tuple.pair
                AUF.all
                PLL.all
                |> List.Nonempty.filter (\( preAUF, pll ) -> not <| params.pll == pll && List.member preAUF equivalentPreAUFs)
                    ( params.preAUF, params.pll )
    in
    allOtherCases
        |> List.Nonempty.toList
        |> List.filter
            (\( preAUF, pll ) ->
                verifySpecForStickers
                    (getRecognitionStickers
                        { params
                            | pll = pll
                            , preAUF = preAUF
                            , pllAlgorithmUsed = getAlgorithmForPLL algorithmIndex pll
                        }
                    )
                    spec
            )


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
                                -- Each group should have all stickers be the same color
                                -- meaning after the uniq only one element should be left in each
                                -- and that color should be opposite to the one of the
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
                                -- Each group should have all stickers be the same color
                                -- meaning after the uniq only one element should be left in each
                                -- and that color should be adjacent to the one of the
                                -- other group
                                ( List.Nonempty.Nonempty firstColor [], List.Nonempty.Nonempty secondColor [] ) ->
                                    areAdjacentColors firstColor secondColor

                                _ ->
                                    False
                       )
                )
        , spec.caseRecognition.identicallyColored
            |> List.all
                (minLength2ToNonemptyList
                    >> List.Nonempty.concatMap getElementStickers
                    >> List.Nonempty.map (getStickerColor stickers)
                    >> List.Nonempty.uniq
                    >> (List.Nonempty.length >> (==) 1)
                )
        , spec.caseRecognition.differentlyColored
            |> List.all
                (\elements ->
                    let
                        list =
                            minLength2ToNonemptyList elements

                        expectedDistinctColors =
                            List.Nonempty.length list

                        allGroupsAreSameColored =
                            list
                                |> List.Nonempty.all
                                    (getElementStickers
                                        >> List.Nonempty.map (getStickerColor stickers)
                                        >> List.Nonempty.uniq
                                        >> List.Nonempty.length
                                        >> (==) 1
                                    )

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



-- Visibility Helpers


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



-- Helpers for removing part from spec


numSpecKeys : Int
numSpecKeys =
    8


removePartKeyIndexRange : Fuzz.Fuzzer Int
removePartKeyIndexRange =
    Fuzz.intRange 0 (numSpecKeys - 1)


removePartFromSpec : Int -> Int -> PLL.RecognitionSpecification -> Maybe ( PLL.RecognitionSpecification, String )
removePartFromSpec keyIndex subIndex ({ caseRecognition } as spec) =
    case modBy numSpecKeys keyIndex of
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



-- Spec utilities


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
extractPatternsFromMinLength2List list =
    list
        |> minLength2ToList
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



-- Helpers for working with the stickers for recognition etc.


type alias RecognitionStickerColors =
    { firstFromLeft : Cube.Advanced.Color
    , secondFromLeft : Cube.Advanced.Color
    , thirdFromLeft : Cube.Advanced.Color
    , firstFromRight : Cube.Advanced.Color
    , secondFromRight : Cube.Advanced.Color
    , thirdFromRight : Cube.Advanced.Color
    }


getRecognitionStickers :
    { pllAlgorithmUsed : Algorithm, recognitionAngle : PLL.RecognitionAngle, preAUF : AUF, pll : PLL }
    -> RecognitionStickerColors
getRecognitionStickers params =
    let
        rotationToGetCorrectRecognitionAngle =
            if params.recognitionAngle == PLL.ufrRecognitionAngle then
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
                    Algorithm.append (AUF.toAlgorithm params.preAUF) params.pllAlgorithmUsed
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



-- Color utilities


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
    (a /= b) && (not <| areOppositeColors a b)



-- data structure simple utilities


minLength2ToList : ( a, a, List a ) -> List a
minLength2ToList ( first, second, tail ) =
    first :: second :: tail


minLength2ToNonemptyList : ( a, a, List a ) -> List.Nonempty.Nonempty a
minLength2ToNonemptyList ( first, second, tail ) =
    List.Nonempty.Nonempty
        first
        (second :: tail)


minLength2FromList : List a -> Maybe ( a, a, List a )
minLength2FromList list =
    case list of
        first :: second :: tail ->
            Just ( first, second, tail )

        _ ->
            Nothing


mapSameForBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapSameForBoth f =
    Tuple.mapBoth f f



-- Hardcoded collections


{-| Generated using the following Cypress code, followed by some semi-manual filtering on straight up
wrong algorithms that were in AlgDB:

it.only("temp", function () {
allPLLs.forEach((pll) => {
const pllString = pllToPllLetters[pll];
cy.visit("<http://algdb.net/puzzle/333/pll/"> + pllString.toLowerCase());
cy.get("td:nth-child(1)").then((nodes) => {
let text = `("${pllString}", [`;
nodes.each((\_, node) => {
text += `"${node.innerText}",`;
});
text +=
"] |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)), ";
cy.log(text);
});
});
});

-}
pllAlgorithms : Dict String (List Algorithm)
pllAlgorithms =
    Dict.fromList
        [ ( "Aa"
          , [ "l' U R' D2 R U' R' D2 R2"
            , "x R' U R' D2 R U' R' D2 R2"
            , "R' F R' B2 R F' R' B2 R2"
            , "y x' R2 D2 R' U' R D2 R' U R' x"
            , "y' x L2 D2 L' U' L D2 L' U L'"
            , "x' R' D R' U2 R D' R' U2 R2"
            , "y' r U r' U' r' F r2 U' r' U' r U r' F'"
            , "y' R U R' F' r U R' U' r' F R2 U' R'"
            , "y2 x' L' U L' D2 L U' L' D2 L2"
            , "R' D' R U2 R' D R U' R' D' R U' R' D R"
            , "y2 L' B L' F2 L B' L' F2 L2"
            , "U' r U r' U' r' F r2 U' r' U' r U r' F'"
            , "y2 r' U L' D2 L U' L' D2 L2"
            , "y' r U r' U' r' F r2 U' r' U' r U r' F'"
            , "R' F R' f2 r U' r' f2 R2"
            , "x' R' U' R U R' D' R U' R' U D R x"
            , "L' U R' D2 R U' R' D2 R L"
            , "y x R2 U2 R' D' R U2 R' D R'"
            , "y2 r' U r' B2 r U' r' B2 r2"
            , "l' U R' u2 L U' L' u2 l R"
            , "y2 R' D' R U' R' D R U' R' D' R U2 R' D R"
            , "y' r U R' F' r U R' U' r' F R2 U' r'"
            , "y R' D' R U' R' D R U2 R' D' R U' R' D R"
            , "U' r U r' U' r' F r2 U' r' U' r U r' F' U2"
            , "x' R' U' R U R' D' R U' R' D U R x"
            , "l' U R' z r2 U R' U' r2 u2"
            , "y' r2 U r2 U' r2 U' D r2 U' r2 U r2 D'"
            , "y R' D' R U' R' D R U2 R' D' R U' R' D R"
            , "R' F l' D2 R U' R' D2 R2"
            , "R2 U D R2 U' R2 U R2 D' R2 U' R2 U R2 U' R2"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Ab"
          , [ "x R2 D2 R U R' D2 R U' R x'"
            , "l' R' D2 R U R' D2 R U' R x'"
            , "y x' R U' R D2 R' U R D2 R2 x"
            , "y' x L U' L D2 L' U L D2 L2"
            , "y l U' R D2 R' U R D2 R2"
            , "R2 B2 R F R' B2 R F' R"
            , "y' r U' L D2 L' U L D2 L2"
            , "x' R2 U2 R D R' U2 R D' R"
            , "y R B' R F2 R' B R F2 R2"
            , "R' D' R U R' D R U R' D' R U2 R' D R"
            , "l' U' l U l F' l2 U l U l' U' l F"
            , "y2 r' L' D2 L U L' D2 L U' L"
            , "y x R D' R U2 R' D R U2 R2"
            , "y' x' L D' L U2 L' D L U2 L2"
            , "y2 R' D' R U2 R' D R U R' D' R U R' D R"
            , "R2 f2 r U r' f2 R F' R"
            , "y' x L U' L D2 L' U L D2 L2"
            , "y' L F' r D2 L' U L D2 r2"
            , "L' U' L F l' U' L U l F' L2 U L"
            , "x' R' D' U' R U R' D R U' R' U R x"
            , "y2 r2 B2 r U r' B2 r U' r"
            , "l' R' D2 R U R' D2 R U' l"
            , "y l U' R z r2 U' R U r2 u2"
            , "R2 D' R2 U R2 U' D R2 U R2 D' R2 D"
            , "y R' D' R U R' D R U2 R' D' R U R' D R"
            , "y' R U R2 U' R' F R U R U' R' F' R U R U' R'"
            , "y' r U' L u2 R' U R u2 r L"
            , "L' U' L F l' U' L U R U' r2 F r"
            , "y x' R U' R z' R2 U' r B R2 B2"
            , "y z U R' D r2 U' R U r2 U' D'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "E"
          , [ "y x' R U' R' D R U R' D' R U R' D R U' R' D' x"
            , "R2 U R' U' y R U R' U' R U R' U' R U R' y' R U' R2"
            , "z U2 R2 F R U R' U' R U R' U' R U R' U' F' R2 U2 z'"
            , "R2 U R2 U D R2 U' R2 U R2 U' D' R2 U R2 U2 R2"
            , "y x' R U' R' D R U R' u2 R' U R D R' U' R x"
            , "y R' U' R' D' R U' R' D R U R' D' R U R' D R2"
            , "F' r U R' U' r' F R U2 r U R' U' r' F R F'"
            , "F R' F' r U R U' r' F R F' r U R' U' r'"
            , "L U' R D2 R' U R L' U' L D2 L' U R'"
            , "y R U R' U R' U' R F' R U R' U' R' F R2 U' R2 U R"
            , "y R2 D' R U' R' D R U' R' D' R U R' D R U R"
            , "R' U L' D2 L U' R L' U R' D2 R U' L"
            , "R U' L D2 L' U R' L U' R D2 R' U L'"
            , "x U R' U' L U R U' L' U R U' L U R' U' r'"
            , "x U R' U' L U R U' r2 U' R U L U' R' U"
            , "l' U' L' U R U' L U R' U' L U R U' L' U"
            , "R2 D R' U2 R D' R' U' R D R' U R D' R' U2 R'"
            , "y R U R D R' U R D' R' U' R D R' U' R D' R2"
            , "L U' R D2 R' U L' R U' L D2 L' U R'"
            , "y x R' U R D' R' U' R D R' U' R D' R' U R D x'"
            , "z U2 R2 F U R U' R' U R U' R' U R U' R' F' R2 U2 z'"
            , "y' R' U' R' D' R U' R' D R U R' D' R U R' D R2"
            , "y l U' R' D R U R' D' R U R' D R U' R' D' x"
            , "L' U R' D2 R U' L R' U L' D2 L U' R"
            , "L R' U' R U L' U' R' U R r U R' U' r' F R F'"
            , "R2 U R' y R U' R' U R U' R' U R U' R' U F U' F2"
            , "y x R' U R' D2 R U' R' D2 R2 z' R' U R' D2 R U' R' D2 R2"
            , "l' U' r' F R F' R U R' U' L U R U' R' F"
            , "y x R D' R' U R D R' U' R D R' U R D' R' U' x'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "F"
          , [ "y R' U' F' R U R' U' R' F R2 U' R' U' R U R' U R"
            , "y2 R' U2 R' d' R' F' R2 U' R' U R' F R U' F"
            , "R' U R U' R2 F' U' F U R F R' F' R2"
            , "M' U2 L F' R U2 r' U r' R2 U2 R2"
            , "R' U R U' R2 y' R' U' R U y x R U R' U' R2 x'"
            , "y2 R' U2 R' U' y R' F' R2 U' R' U R' F R U' F"
            , "y' L U F L' U' L U L F' L2 U L U L' U' L U' L'"
            , "y R2 F R F' R' U' F' U F R2 U R' U' R"
            , "R U R' U' R' U R U2 L' R' U R U' L U' R U' R'"
            , "M U x' R2 U2 R2 r U' r U2 l' U r'"
            , "M' U2 r U' R x' U2 r' U R2 r' U2 R' l'"
            , "R U R' U L' U L U2 R' L' U L U' R U R U' R'"
            , "y' z D' U2 R' U' R' U2 R U R U2 R U' R2 D R' U"
            , "r U R' U' z U' l' U2 r' U' r U2 l' U l' R'"
            , "y2 R' U2 R' U' R D' R' D R' U D' R2 U' R2 D R U' R"
            , "y R2 U R2 U D R2 U2 D' R2 U2 R2 U R2 D R2 U' D' R2"
            , "y' x D' l2 D' l' z' R' F' R2 U' R' U R' F R U' f"
            , "y x U R2 F R2 U' R' U R' x U2 r U' r' U l R U' x'"
            , "y' F r2 R' U2 r U' r' U2 l R U' R' U r2 u'"
            , "y2 R' U2 R' U' x2 y' R' U R' U' l2 F' R' F R U' F"
            , "y R U2 R' U' r U2 R' F R U2 r2 F R2 U2 r' M2"
            , "y2 R U' R' U R2 y R U R' U' x U' R' U R U2"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Ga"
          , [ "R2 u R' U R' U' R u' R2 y' R' U R"
            , "R2 U R' U R' U' R U' R2 D U' R' U R D'"
            , "R2 u R' U R' U' R u' R2 F' U F"
            , "D' R2 U R' U R' U' R U' R2 U' D R' U R"
            , "y R U R' F' R U R' U' R' F R U' R' F R2 U' R' U' R U R' F'"
            , "R2 u R' U R' U' R u' R2 y L' U L"
            , "L2 F2 L' U2 L' U2 L F' L' U' L U L F' L2"
            , "R U R' U' R' U F R U R U' R' F' U R' U2 R"
            , "F2 R2 L2 U R2 U' R2 D R2 D' L2 F2"
            , "y2 R U' R U R2 D R' U' R D' R' U2 R U' R' U' R2"
            , "y2 L2 u L' U L' U' L u' L2 y' L' U L"
            , "r U2 R U' r' F U R' U' F' r U' r'"
            , "R2 S2 U l2 U' l2 u R2 U' r2 F2"
            , "y' R U2 R' U' F' R U R2 U' R' F R U R2 U2 R'"
            , "y2 z U2 r U' R U' R' U r' U2 x' U' R U x z'"
            , "y2 R l U2 l' U2 R' U2 l U' R' F' R F R U' R2"
            , "R2 u R' U R' U' R u' R2 b' R b"
            , "y' u' L2 U L' U L' U' L U' L2 U' D L' U L"
            , "R2 u R' U R' U' R u' R2 b' R F"
            , "y' R L U2 R' L' F' U B' U2 F U' B"
            , "y2 x' R2 U2 R' F2 R' F2 R U' R' F' R F R U' R2"
            , "L U' R' U x U2 r' R U R' U' r U2 r' U' R"
            , "y2 L2 U L' U L' U' L U' L2 U' D L' U L D'"
            , "R2 u R' U R' U' R u' R2 z x U' R U"
            , "R2 U' R2 U R2 U' D' R2 U2 R2 U' R2 U R2 U2 R2 U' D"
            , "l U2 R' r U r' F' D R U' R' D' R U' l'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Gb"
          , [ "R' U' R U D' R2 U R' U R U' R U' R2 D"
            , "R' U' R y R2 u R' U R U' R u' R2"
            , "y F' U' F R2 u R' U R U' R u' R2"
            , "R' d' F R2 u R' U R U' R u' R2"
            , "D R' U' R U D' R2 U R' U R U' R U' R2"
            , "y R U R' F' r U R' U' r' F R F' R U R' U' R' F R2 U' R'"
            , "y2 L' U' L y' R2 u R' U R U' R u' R2"
            , "y2 R' U2 R U' F R U R' U' R' F' U' R U R U' R'"
            , "y2 L' U' L y L2 u L' U L U' L u' L2"
            , "y r U r' F U R U' F' r U R' U2 r'"
            , "R' U' y F R2 u R' U R U' R u' R2"
            , "R' U' R U D' R2 U R' U R U' R U' R2 u"
            , "y2 L' U' L U D' L2 U L' U L U' L U' L2 D"
            , "y' l U R' D R U R' D' F r U' r' R U2 l'"
            , "y2 L' U' L U D' L2 U L' U L U' L U' L2 D"
            , "R' U' R B2 D L' U L U' L D' B2 U2"
            , "y' R' U L' U2 R U' L y' L R U2 L' R'"
            , "y' u R' U' R U D' R2 U R' U R U' R U' R2"
            , "y u L' U' L U D' L2 U L' U L U' L U' L2"
            , "y u' L' U' L U D' L2 U L' U L U' L U' L2 D2"
            , "L' U R' U2 L U' R y' L R U2 L' R'"
            , "R' U r U2 r' U R U' R' r U2 x' U' R U L'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Gc"
          , [ "R2 u' R U' R U R' u R2 y R U' R'"
            , "y2 R2 F2 R U2 R U2 R' F R U R' U' R' F R2"
            , "R2 U' R U' R U R' U R2 D' U R U' R' D"
            , "R2 u' R U' R U R' u R2 f R' f'"
            , "y2 L2 u' L U' L U L' u L2 y L U' L'"
            , "D R2 U' R U' R U R' U R2 U D' R U' R'"
            , "R2 u' R U' R U R' u R2 B U' B'"
            , "R2 u' R U' R U R' D x' U2 r U' r'"
            , "y2 L2 U' L U' L U L' U L2 D' U L U' L' D"
            , "y2 L2 u' L U' L U L' u L2 y' R U' R'"
            , "y2 R2 F2 R U2 R r' F r U2 R' U' F2 R"
            , "y F2 D' L U' L U L' D F2 R U' R'"
            , "R2 F2 R U2 M F r U2 R' U' F2 R"
            , "R2 S2 U' l2 U l2 u' R2 U r2 B2"
            , "R2 U' R U' R U R' U R2 D' U R U' R' u"
            , "D R2 U' R U' R U R' U R2 D' U R U' R'"
            , "R2 u' R U' R U R' u R2 f R' f'"
            , "y2 L2 u' L U' L U L' u L2 F U' F'"
            , "U' L' R' U2 L R F U' B U2 F' U B'"
            , "U' B2 D' R U' R U R' D B2 L U' L'"
            , "y L' U' L F L' U' L U L F' L' U L F' L2 U L U L' U' L F"
            , "y' R' L' U2 R L y' R U' L U2 R' U L' U2"
            , "y' L' R' U2 L R y L U' R U2 L' U R'"
            , "y' u R2 U' R U' R U R' U R2 D' U R U' R'"
            , "y u L2 U' L U' L U L' U L2 D' U L U' L'"
            , "y F2 R2 L2 U' L2 U L2 D' L2 D R2 F2"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Gd"
          , [ "R U R' U' D R2 U' R U' R' U R' U R2 D'"
            , "R U R' y' R2 u' R U' R' U R' u R2"
            , "D' R U R' U' D R2 U' R U' R' U R' U R2"
            , "f R f' R2 u' R U' R' U R' u R2"
            , "R U R' F' R U R' U R U' R' U' R' F R2 U R' U' R U' R'"
            , "y R2 F' R U R U' R' F' R U2 R' U2 R' F2 R2"
            , "y2 L U L' B2 D' R U' R' U R' u R2"
            , "y2 L U L' U' D L2 U' L U' L' U L' U L2 D'"
            , "y2 L U L' y' L2 u' L U' L' U L' u L2"
            , "y F U F' L2 u' L U' L' U L' u L2"
            , "y2 L U r' U2 x D' R U' R' U R' u R2"
            , "F2 R2 D' L2 D L2 U' L2 U M2 B2"
            , "R U R' y L2 u' L U' L' U L' u L2"
            , "y2 x' r U r' U2 x D' R U' R' U R' u R2"
            , "y R U' L U2 R' U L' y' L' R' U2 L R"
            , "R U R' D U' R2 U' R U' R' U R' U R2 u'"
            , "y u' R U R' U' D R2 U' R U' R' U R' U R2"
            , "R U R' y' R2 u' R U' R' U R' D B2"
            , "y' u' L U L' U' D L2 U' L U' L' U L' U L2"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "H"
          , [ "M2 U M2 U2 M2 U M2"
            , "M2 U' M2 U2 M2 U' M2"
            , "R2 U2 R U2 R2 U2 R2 U2 R U2 R2"
            , "M2 U' M2 U2 M2 U' M2"
            , "R2 U2 R2 U2 R2 U' R2 U2 R2 U2 R2"
            , "M2 U2 M2 U' M2 U2 M2"
            , "R2 U2 R' U2 R2 U2 R2 U2 R' U2 R2"
            , "M2 u' M2 u2 M2 u' M2"
            , "M2 u M2 u2 M2 u M2"
            , "M2 U M2 U M2 U M2 U M2 U M2"
            , "R L U2 L' R' y L' R' U2 R L"
            , "M2 U2 M2 U M2 U2 M2"
            , "S R U2 R2 U2 R2 U2 R S'"
            , "x' R r U2 r' R' U' u' R2 U D"
            , "R U R' U2 R U' R' U' R U' R' U2 R U' R' U' R U2 R'"
            , "M' U M2 U2 M2 U M2 U2 M'"
            , "R U F' R U R' U R2 U2 R' F U' R' U R' U2 R"
            , "F B' U' R2 U2 R2 U2 R2 U' F' B"
            , "M' U' M2 U2 M2 U' M2 U2 M'"
            , "r' R y' R U2 R2 U2 R2 U2 R S'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Ja"
          , [ "y R' U L' U2 R U' R' U2 R L"
            , "L' U' L F L' U' L U L F' L2 U L"
            , "y2 x R2 F R F' R U2 r' U r U2 x'"
            , "y2 R' U2 R U R' U2 L U' R U L'"
            , "L' U2 L U L' U2 R U' L U R'"
            , "y2 F U' R' F R2 U' R' U' R U R' F' R U R' F'"
            , "x U2 r' U' r U2 l' U R' U' R2"
            , "L' U R' z R2 U R' U' R2 U D"
            , "y' z U' R D' R2 U R' U' R2 U D R' z'"
            , "L U' R' U L' U2 R U' R' U2 R"
            , "y2 R' U2 R U R' z R2 U R' D R U'"
            , "R U' L' U R' U2 L U' L' U2 L"
            , "y2 L U' R' U L' U2 R U' R' U2 R"
            , "F U' R' F R2 U' R' U' R U R' F' R U R' F'"
            , "y2 F2 L' U' r U2 l' U R' U' R2"
            , "R2 F2 U' F2 D R2 D' R2 U R2"
            , "y R' U' R' D R2 U' R' U R2 D' R2 U R"
            , "y2 R' F R F' r U R' U' r' U2 R U R' U R"
            , "R2 U' D R2 U' R2 U R2 D' R2 U R2"
            , "L' R' U2 R U R' U2 L U' R"
            , "y x' R2 u' R' u R2 x' y' R' U R' U' R2"
            , "r2 U D' r2 U' r2 D r2 D' r2 D r2"
            , "l' R' F R F' R U2 r' U r U2 x'"
            , "R2 U' R2 D R2 U' R2 U R2 U D' R2"
            , "y2 R' U2 R U R' U2 L U' R U L'"
            , "R' U2 z D R D' R2 U R' D R U'"
            , "y2 R2 U' R2 D R2 U' R2 U R2 U D' R2"
            , "y2 l D l' U l D' l' U2 l D l' U l D' l'"
            , "y2 R2 U' R' U R' U' R' F R2 U' R' U' R U R' F' R2 U R2"
            , "z D' R2 D R D' R2 U R' D R U'"
            , "y2 R' U' R B R' U' R U R B' R2 U R U"
            , "y2 r U' r' U' r U r D r' U' r D' r' U2 R' U' M"
            , "r U r' U2 r' D' r U' r' D r U' r U' r'"
            , "U2 L U' R' U L' U2 R U' R' U2 R U2"
            , "y F2 L' U' r U2 l' U R' U' l2"
            , "y2 F2 D' L2 D L2 U' L2 U L2 F2 U"
            , "D' R2 U2 R2 D R2 U2 D' R2 U R2 U R2 U' R2 D R2"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Jb"
          , [ "R U R' F' R U R' U' R' F R2 U' R'"
            , "R U2 R' U' R U2 L' U R' U' L"
            , "R U2 R' U' R U2 L' U R' U' r x"
            , "L' U R U' L U2 R' U R U2 R'"
            , "y' L U' R U2 L' U L U2 R' L'"
            , "y R U' L U2 R' U R U2 L' R'"
            , "y R2 U D' R2 U R2 U' R2 D R2 U' R2"
            , "R L U2 R' U' R U2 L' U R'"
            , "R2 U R2 U R2 U' R2 D R2 D' R2 U' R2 D R2 D'"
            , "r' F R F' r U2 R' U R U2 R'"
            , "B2 L U L' B2 R D' R D R2"
            , "y2 R L U2 L' U' L U2 R' U L' U"
            , "y2 R' U2 R U R' F' R U R' U' R' F R2 U' R' U R"
            , "y' L r U2 R2 F R F' R U2 r' U L'"
            , "y2 r' D' r U' r' D r U2 r' D' r U' r' D r"
            , "y2 R' U L U' R U2 L' U L U2 L'"
            , "R U l' U' l U R' U' l' U l R U' R' U'"
            , "y2 R L U2 L' U' L U2 R' U L'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Na"
          , [ "R U R' U R U R' F' R U R' U' R' F R2 U' R' U2 R U' R'"
            , "L U' R U2 L' U R' L U' R U2 L' U R'"
            , "z U R' D R2 U' R D' U R' D R2 U' R D' z'"
            , "r' D r U2 r' D r U2 r' D r U2 r' D r U2 r' D r"
            , "R U' L U2 R' U L' R U' L U2 R' U L'"
            , "F' R U R' U' R' F R2 F U' R' U' R U F' R'"
            , "z R' U R' D R2 U' R U D' R' D R2 U' R D' z'"
            , "R U R' U R U2 R' U' R U2 L' U R' U' L U' R U' R'"
            , "z D R' U R2 D' R U' D R' U R2 D' R U' z'"
            , "L U' L' U L F U F' L' U' L F' L F L' U L'"
            , "R U2 D' R U2 R' U' D R2 U' D' R U R' D R2"
            , "L U' R U2 r' F M' U' R U2 r' F l'"
            , "L U' R U2 L' U R' L U' R U2 L' U R' U'"
            , "R U' R' U l U F U' R' F' R U' R U R' F R'"
            , "F' R2 U R2 U' R2 F U2 F' R2 U R2 U' R2 F"
            , "R' U R2 B2 U R' B2 R U' B2 R2 U' R U'"
            , "R U2 R U2 R' U2 D R' U R2 D' R D R2 U' D'"
            , "R U R' U r' F R F' r U2 R' U R U2 R' U' R U' R'"
            , "z2 L D L' D L D L' F' L D L' D' L' F L2 D' L' D2 L D' L'"
            , "R2 D r' U2 r D' R' U2 R' U2 F R U' R' U' R U2 R' U' F'"
            , "x L' U' L B2 L' U' L B2 L' U' L B2 L' U' L B2 L' U' L x'"
            , "R U R' U R L U2 R' U' R U2 L' U R' U2 R U' R'"
            , "l F' R' U' R z' R U' R' U' r U l' U R' U' R2"
            , "L R U' R' U L' U2 R U' R2 D' r U2 r' D R"
            , "R' U R U' R' L' D2 L U2 L' D2 L R U R' U' R"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Nb"
          , [ "R' U R U' R' F' U' F R U R' F R' F' R U' R"
            , "R' U L' U2 R U' L R' U L' U2 R U' L"
            , "z D' R U' R2 D R' U D' R U' R2 D R' U z'"
            , "r D r' U2 r D r' U2 r D r' U2 r D r' U2 r D r'"
            , "L' U' L U' L' U' L F L' U' L U L F' L2 U L U2 L' U L"
            , "r' D' F r U' r' F' D r2 U r' U' r' F r F'"
            , "z U' R D' R2 U R' D U' R D' R2 U R' D z'"
            , "R' U R' F R F' R U' R' F' U F R U R' U' R"
            , "L' U R' U2 L U' R L' U R' U2 L U' R"
            , "l D' l' U2 l D' l' U2 l D' l' U2 l D' l' U2 l D' l'"
            , "R' U L' U2 R U' M' B r' U2 R U' L"
            , "R' U R U' R' F' U' F R U R' U' R U' f R f'"
            , "R' U R U' R' F' U' F R U R' U' R d' R U R'"
            , "L' U' L R' U L' U2 R U' R' U2 L R U' L' U L"
            , "z U' R2 U R U R' F' R U R' U' R' F R2 U' R' U2 R2 U"
            , "R U' R2 F2 U' R F2 R' U F2 R2 U R'"
            , "r D' r' U2 r D' r' U2 r D' r' U2 r D' r' U2 r D' r'"
            , "R' U' R U' R' L' U2 R U R' U2 L U' R U2 R' U R"
            , "L' U R' U2 L U' L' R U R' U2 L U' R"
            , "B R2 U' R2 U R2 B' U2 B R2 U' R2 U R2 B'"
            , "r' D' F r U' r' F' D r2 U r' U' L' U r U'"
            , "R' U2 D R' U2 R U D' R2 U D R' U' R D' R2"
            , "R' U' R U' x R2 F R F' R U2 r' U r U2 x' U R' U R"
            , "L U' L2 B2 U' L B2 L' U B2 L2 U L' U"
            , "L' U' L U R' U2 R U R' U2 z U R' D R U' R' U' R U z'"
            , "L' R' U R U' L U2 R' U R2 D r' U2 r D' R'"
            , "z U' R' U D' R U' R2 D R' D' R2 U D R' U' R U z'"
            , "L' U' L U L U' R' U L' U2 R U' R' U2 R U' L' U L"
            , "r' U R U' R' U r' F' r U r U2 r' F2 U2 r"
            , "D' U R2 D R D' R2 U R' D U2 R' U2 R U2 R"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Ra"
          , [ "y R U R' F' R U2 R' U2 R' F R U R U2 R'"
            , "L U2 L' U2 L F' L' U' L U L F L2"
            , "y R U' R' U' R U R D R' U' R D' R' U2 R'"
            , "y2 R U2 R' U2 R B' R' U' R U R B R2"
            , "y2 R U2 R' U' R' F' R U2 R U2 R' F R U' R'"
            , "R U R' F' U' F R U R' F R' F' R2 U2 R'"
            , "y' L2 F' L' U' L' U L F L' U2 L U2 L'"
            , "y2 R U' R2 D' R U R' D R U' R U' R' U R U R'"
            , "y2 R U2 R D R' U R D' R' U' R' U R U R'"
            , "R2 F2 U R U R' U' R' U' F2 R' U R' U'"
            , "y R U' R' U' R U R' U R' D' R U' R' D R2 U R'"
            , "y2 R U2 R' U2 l U' l' U' R U l U R2"
            , "y R l U' l' U' R' U l U l' U2 R U2 R'"
            , "R U2 R2 F R F' R U' R' F' U F R U' R'"
            , "y2 R U' R F2 U R U R U' R' U' F2 R2"
            , "y' z U2 R2 U' r' U r U' r' U' r d' R U' R' U' F2 U2"
            , "y' L U' L' U' L U L D L' U' L D' L' U2 L'"
            , "y2 R U2 R' U2 R B' R' U' R U R B R2"
            , "L U2 L D L' U L D' L' U' L' U L U L'"
            , "L U2 L' U F R' F' L F R F' U L'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Rb"
          , [ "R' U2 R U2 R' F R U R' U' R' F' R2"
            , "R' U2 R' D' R U' R' D R U R U' R' U' R"
            , "y R2 F R U R U' R' F' R U2 R' U2 R"
            , "y' R U2 R' U2 R' F R2 U' R' U' R U R' F' R U R' U R U2 R'"
            , "y2 r' F2 r F R U' R' U' F' U' r U' r' F"
            , "y' L' U L U L' U' L' D' L U L' D L U2 L"
            , "y2 R U R' U' f' U2 F2 R U R' U' F2 U2 f"
            , "y R' U R U R' U' R' D' R U R' D R U2 R"
            , "y2 R U R' U2 r' U2 R' f' U' f R2 U2 r"
            , "y2 L' U2 L' D' L U' L' D L U L U' L' U' L"
            , "R' U2 R U' x U' r x' U R' U' L' U x' U' R"
            , "y2 L' U L' F2 U' L' U' L' U L U F2 L2"
            , "y2 x r' U r' D2 r U' r' D2 r2 x' U R' L F2 L' R"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "T"
          , [ "R U R' U' R' F R2 U' R' U' R U R' F'"
            , "R U R' U' R' F R2 U' R' U F' L' U L"
            , "R2 U R2 U' R2 U' D R2 U' R2 U R2 D'"
            , "y2 L' U' L U L F' L2 U L U L' U' L F"
            , "y F2 D R2 U' R2 F2 D' r2 D r2"
            , "R2 u R2 U' R2 F2 D' r2 D r2"
            , "R U R' U' R U' R' U' R U R' F' R U R' U' R' F R U R U' R'"
            , "D' R2 U R2 U' R2 U' D R2 U' R2 U R2"
            , "R U R' U' R2 D R' U' R' U' R U R2 D' R"
            , "y F2 D R2 U' R2 F2 D' L2 U L2 U'"
            , "R2 u' R2 U R2 y R2 u R2 U' R2"
            , "y' R' U R' F2 r F' r' F2 R2 U' M U2 M'"
            , "R U' R' U2 L R U' R' U' R' U2 R U2 L' U' R' U R"
            , "y' M' R' U R' F2 r F' r' F2 R2 U' M"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Ua"
          , [ "y2 R U' R U R U R U' R' U' R2"
            , "R2 U' R' U' R U R U R U' R"
            , "y2 M2 U M U2 M' U M2"
            , "M2 U M' U2 M U M2"
            , "R U R' U R' U' R2 U' R' U R' U R"
            , "y R2 U' S' U2 S U' R2"
            , "y2 F2 U' L R' F2 L' R U' F2"
            , "y M2 u' M' u2 M' u' M2"
            , "L U' L U L U L U' L' U' L2"
            , "y R2 U S R2 S' R2 U' R2"
            , "y R2 U' F B' R2 B F' U' R2"
            , "R U R' U' L' U' L U2 R U' R' U' L' U L"
            , "y2 L2 U' L' U' L U L U L U' L"
            , "y R U R' U' R' U2 R U R U' R2 U2 R"
            , "r U r' R U R' U' M' U R U2 r'"
            , "y' M2 u M' u2 M' u2 M' u2 M' u M2"
            , "y R2 D' M' U2 M U2 D R2"
            , "y2 F2 U' M' U2 M U' F2"
            , "y R U2 R U R U R2 U' R' U' R2"
            , "y2 L U L' U L' U' L2 U' L' U L' U L"
            , "y F U R U' R' F' U2 F' L' U' L U F"
            , "y2 M' U2 M U M' U2 M U M' U2 M"
            , "y' R U R2 U' R' U' R U R U R U' R U' R'"
            , "y F U R U' R' F' f' U' L' U L f"
            , "y' R' U' R U' R U R U R U' R' U' R2 U R"
            , "R' U' R U' R' U2 R U' L U L' U L U2 L'"
            , "y R2 U' F B' R2 F' B U' R2"
            , "M U2 M' U M U2 M' U M U2 M'"
            , "y F U R U' R' F' y2 F' L' U' L U F"
            , "y' M2 u' M u2 M u' M2"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Ub"
          , [ "y2 R2 U R U R' U' R' U' R' U R'"
            , "y2 M2 U' M U2 M' U' M2"
            , "R' U R' U' R' U' R' U R U R2"
            , "M2 U' M' U2 M U' M2"
            , "y2 L' U L' U' L' U' L' U L U L2"
            , "y' M2 u M' u2 M' u M2"
            , "y2 F2 U L R' F2 L' R U F2"
            , "y R2 U' S R2 S' R2 U R2"
            , "L2 U L U L' U' L' U' L' U L'"
            , "y2 R' U' R U' R U R2 U R U' R U' R'"
            , "y R2 U F B' R2 F' B U R2"
            , "y R2 U R2 S R2 S' U' R2"
            , "L U L' U L U2 L' U R' U' R U' R' U2 R U'"
            , "L' U' L U R U R' U2 L' U L U R U' R'"
            , "y' M2 U' M2 U2 M' U2 M' U M2"
            , "R U R' F' R U R' U' M' U R U' r' F R U' R'"
            , "L' U' L U' L U L2 U L U' L U' L'"
            , "L' U' L U R U R' U2 L' U L U R U' R'"
            , "M U2 M' U' M U2 M' U' M U2 M'"
            , "y M2 U' M2 U2 M' U2 M' U M2"
            , "L' U' L U' L U L2 U L U' L U' L'"
            , "y R' U2 R2 U R' U' R' U2 R U R U' R'"
            , "y' R U R' U R' U' R' U' R' U R U R2 U' R'"
            , "l' U' l L' U' L U M' U' L' U2 l"
            , "y' R' U' R2 U R U R' U' R' U' R' U R' U R"
            , "R U R' U' R' U' R' U R d' M' U2 M"
            , "y R2 U R U R2 U' R' U' R' U2 R'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "V"
          , [ "R' U R' d' R' F' R2 U' R' U R' F R F"
            , "R' U R' U' y R' F' R2 U' R' U R' F R F"
            , "R' U R' U' R D' R' D R' U D' R2 U' R2 D R2"
            , "z D' R2 D R2 U R' D' R U' R U R' D R U'"
            , "R U2 R' D R U' R U' R U R2 D R' U' R D2"
            , "y2 R U' R U R' D R D' R U' D R2 U R2 D' R2"
            , "R' U2 R U2 L U' R' U L' U L U' R U L'"
            , "R2 U' B2 U B2 R D' R D R' U R U' R"
            , "R2 D' R2 U R2 U' D R D' R D R' U R U' R"
            , "R' U R U' x' U R U2 R' U' R U' R' U2 R U R' U' x"
            , "y L' U R U' L U L' U R' U' L U2 R U2 R'"
            , "y2 R U' L' U R' U' R U' L U R' U2 L' U2 L"
            , "R2 U' f2 D f2 R D' R D R' U R U' R"
            , "y R F R2 U' R' U R' U' R2 U R F' R' U' R U R'"
            , "l' U R' D2 R U' R' D2 F R F' R U2 r' U r U2 x'"
            , "R' U R' U' R D' R' D R' B2 U' B2 U R2"
            , "R' U l' f' l' U l' B' l2 U' R' U R U"
            , "y R U R' U R' U' F' R U R' U' R' F R2 U' R' U' R U R' U R U' R U' R'"
            , "R' U R' U' y x2 R' U R' U' x' R2 U' R' U R U"
            , "R' U R' U' R D' R' D R' y R2 U' R2 d R2"
            , "y R U R' U R U' R' U L' U2 R U2 R' U2 L R U2 R' U"
            , "R' U R' U' R D' R' D R' y R2 U' R2 U F2"
            , "R' U R' U' y x R' F R' F' R2 U' R' U R U"
            , "y' R' U L U' R U R' U L' U' R U2 L U2 L'"
            , "y R U2 R' U2 L' U R U' L U' L' U R' U' L"
            , "y2 L' U2 L U2 R U' L' U R' U R U' L U R'"
            , "y z U' R D R' U R U' R D' R' U R2 D R2 D' z'"
            , "y' z D' R U R' D R D' R U' R' D R2 U R2 U' z'"
            , "y' L U2 L' U2 R' U L U' R U' R' U L' U' R"
            , "R' U R' U' y x R' U' R2 B' R' B R' U R U"
            , "F R' U' R' F' U' F R U2 R U' R' F R2 F' R' F'"
            , "R U R D R' U' R D' R2 U2 R' U2 R' D' r U2 r' D R2"
            , "y' R U2 R u2 R U' R' D' R U R2 F2 R D' L2"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Y"
          , [ "F R U' R' U' R U R' F' R U R' U' R' F R F'"
            , "F R' F R2 U' R' U' R U R' F' R U R' U' F'"
            , "R2 U' R2 U' R2 U R' F' R U R2 U' R' F R"
            , "R2 D' R2 U R2 U' R2 D R2 U' R2 U R2 U R2"
            , "y2 R' U2 R' F' R2 U' R' F' U' F R U R' F U2 R"
            , "R' U' R F2 R' U R d R2 U' R2 U' R2"
            , "F R U' R' U' R U y' R U R' B' R U' R2"
            , "y F' L' U L U L' U' L F L' U' L U L F' L' F"
            , "R2 U' R' U R U' y' x' L' U' R U' R' U' L U"
            , "R2 U' R2 U' R2 U y' R U R' B2 R U' R'"
            , "F R U' R' U' R d R U R' B' R U' R2"
            , "F U F' R2 F U' F' U' R2 U R2 U R2"
            , "F R U r U2 R2 F R F' R U2 r' R' F'"
            , "R' U' R U' R U R' F' R U R' U' R' F R2 U' R2 U R"
            , "R' U' l D2 l' U R d R2 U' R2 U' R2"
            , "y' x' U' R U l' U' R' U R2 U R' U' R' F R F'"
            , "R2 U' R' U R U' x' U' z' U' R U' R' U' z U R x"
            , "R2 U' R' U R U' x' U' z' U' R U' R' U' L U x"
            , "F R U R U2 R' L' U R U' L U2 R2 F'"
            , "F R' F' R U R U' R' F R U' R' U R U R' F'"
            , "R' F' R U R' U' R' F R U' R U R2 D' R U R' D R2"
            , "x U R' U l R U' R' U' R U R' F' R U R' U' F'"
            , "F R' F' R U R U' R2 U' R U l U' R' U x"
            , "F R U r U2 R2 F R F' R U2 r' R' F'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        , ( "Z"
          , [ "M2 U M2 U M' U2 M2 U2 M'"
            , "y M2 U' M2 U' M' U2 M2 U2 M'"
            , "M' U' M2 U' M2 U' M' U2 M2"
            , "y R' U' R U' R U R U' R' U R U R2 U' R'"
            , "R' U' R2 U R U R' U' R U R U' R U' R'"
            , "y M' U M2 U M2 U M' U2 M2"
            , "R U R' U R' U' R' U R U' R' U' R2 U R"
            , "M2 U' M' U2 M2 U2 M' U M2"
            , "M2 U2 M U' M2 U' M2 U' M"
            , "M2 u M2 u' S M2 S'"
            , "M2 U M2 U' E2 M E2 M"
            , "R2 U R2 U2 R2 U R2 U' R2 U R2 U2 R2 U R2"
            , "M2 U2 M' U' M2 U' M2 U' M'"
            , "y M2 U' M2 U' M' U2 M2 U2 M' U2"
            , "M2 U M2 U M' U2 M2 U2 M'"
            , "R2 U R2 U R U2 R2 U2 R2 U2 R U' R2 U' R2"
            , "M2 U M2 U x' U2 M2 U2 M2"
            , "S2 D' M E2 M D' U2 S2"
            , "M2 U2 M' U M2 U M2 U M'"
            , "y M2 U' M2 U' M U2 M2 U2 M U2"
            , "F' L' U' L U F R' U' F R' F' R U R"
            , "y F R U R' U' F' L U F' r U r' U' L'"
            , "M' U2 M2 U2 M' U' M2 U' M2"
            , "y R U R2 U' R' U' R U R' U' R' U R' U R"
            , "M2 U M2 U x M2 U2 M2 U2"
            , "M' U2 M2 U2 M' U' M2 U' M2"
            , "x' R U' R' U D R' D U' R' U R D2"
            , "R2 U' R2 U' R2 U' R2 U' R2 U' S R2 S'"
            ]
                |> List.map (Algorithm.fromString >> Result.withDefault Algorithm.empty)
          )
        ]
