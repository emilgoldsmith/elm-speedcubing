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
import PLL exposing (PLL, RecognitionPattern(..), Sticker(..))
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


testedPlls : Fuzz.Fuzzer PLL
testedPlls =
    Fuzz.oneOf (List.map Fuzz.constant [ PLL.E, PLL.T, PLL.Ga ])


getUniqueTwoSidedRecognitionSpecificationTests : Test
getUniqueTwoSidedRecognitionSpecificationTests =
    only <|
        describe "getUniqueTwoSidedRecognitionSpecificationTests"
            [ fuzz (Fuzz.tuple ( aufFuzzer, testedPlls )) "no patterns (except absent ones) mentioned that are not included in the patterns" <|
                \arg ->
                    let
                        spec =
                            PLL.getUniqueTwoSidedRecognitionSpecification
                                PLL.referenceAlgorithms
                                arg

                        patterns =
                            spec.patterns
                                |> Maybe.map List.Nonempty.toList
                                |> Maybe.withDefault []

                        otherPatterns =
                            extractAllPatterns { spec | patterns = Nothing, absentPatterns = Nothing }
                    in
                    List.all (\x -> List.member x patterns) otherPatterns
                        |> Expect.true ("There was a pattern mentioned not included in patterns. The spec was: " ++ Debug.toString spec)
            , fuzz (Fuzz.tuple ( aufFuzzer, testedPlls )) "no patterns mentioned that are included in absent patterns" <|
                \arg ->
                    let
                        spec =
                            PLL.getUniqueTwoSidedRecognitionSpecification
                                PLL.referenceAlgorithms
                                arg

                        absentPatterns =
                            spec.absentPatterns
                                |> Maybe.map List.Nonempty.toList
                                |> Maybe.withDefault []

                        otherPatterns =
                            extractAllPatterns { spec | absentPatterns = Nothing }
                    in
                    List.all (\x -> not <| List.member x absentPatterns) otherPatterns
                        |> Expect.true ("There was a pattern mentioned that was also included in absent patterns. The spec was: " ++ Debug.toString spec)

            -- This one also ensures that it's internally coherent as otherwise
            -- it wouldn't describe the case correctly if for example a sticker
            -- is supposed to be two different colors
            , fuzz (Fuzz.tuple ( aufFuzzer, testedPlls )) "the spec matches the case" <|
                \arg ->
                    let
                        spec =
                            PLL.getUniqueTwoSidedRecognitionSpecification
                                PLL.referenceAlgorithms
                                arg

                        stickers =
                            getRecognitionStickers
                                PLL.referenceAlgorithms
                                arg
                    in
                    verifySpecForStickers stickers spec
                        |> Expect.true
                            ("the spec didn't correctly describe the stickers. The spec was:\n"
                                ++ Debug.toString spec
                                ++ "\nThe stickers were:\n"
                                ++ Debug.toString stickers
                            )
            , fuzz (Fuzz.tuple ( aufFuzzer, testedPlls )) "check that no other cases except for symmetric ones match this spec; that it's therefore uniquely determinable by this description" <|
                \( preAUF, pll ) ->
                    let
                        spec =
                            PLL.getUniqueTwoSidedRecognitionSpecification
                                PLL.referenceAlgorithms
                                ( preAUF, pll )

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
                                    (getRecognitionStickers PLL.referenceAlgorithms otherCase)
                                    spec
                            )
                        |> Expect.equalLists []
            , todo "include display angles"
            , todo "include postAUF"
            , todo "include non-reference algorithms"
            ]


type alias RecognitionStickerColors =
    { firstFromLeft : Cube.Advanced.Color
    , secondFromLeft : Cube.Advanced.Color
    , thirdFromLeft : Cube.Advanced.Color
    , firstFromRight : Cube.Advanced.Color
    , secondFromRight : Cube.Advanced.Color
    , thirdFromRight : Cube.Advanced.Color
    }


getRecognitionStickers : PLL.Algorithms -> ( AUF, PLL ) -> RecognitionStickerColors
getRecognitionStickers algorithms ( preAUF, pll ) =
    Cube.solved
        |> Cube.applyAlgorithm
            (Algorithm.inverse <|
                Algorithm.append (AUF.toAlgorithm preAUF) <|
                    PLL.getAlgorithm algorithms pll
            )
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
                FirstStickerFromLeft
                [ FirstStickerFromRight ]

        PLL.LeftHeadlights ->
            List.Nonempty.Nonempty
                FirstStickerFromLeft
                [ ThirdStickerFromLeft ]

        PLL.RightHeadlights ->
            List.Nonempty.Nonempty
                FirstStickerFromRight
                [ ThirdStickerFromRight ]

        PLL.LeftThreeBar ->
            List.Nonempty.Nonempty
                FirstStickerFromLeft
                [ SecondStickerFromLeft
                , ThirdStickerFromLeft
                ]

        PLL.RightThreeBar ->
            List.Nonempty.Nonempty
                FirstStickerFromRight
                [ SecondStickerFromRight
                , ThirdStickerFromRight
                ]

        PLL.LeftInsideTwoBar ->
            List.Nonempty.Nonempty
                SecondStickerFromLeft
                [ ThirdStickerFromLeft ]

        PLL.RightInsideTwoBar ->
            List.Nonempty.Nonempty
                SecondStickerFromRight
                [ ThirdStickerFromRight ]

        PLL.LeftOutsideTwoBar ->
            List.Nonempty.Nonempty
                FirstStickerFromLeft
                [ SecondStickerFromLeft ]

        PLL.RightOutsideTwoBar ->
            List.Nonempty.Nonempty
                FirstStickerFromRight
                [ SecondStickerFromRight ]


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
verifySpecForStickers stickers nonFinalSpec =
    let
        finalSpec =
            { nonFinalSpec
                | absentPatterns =
                    (nonFinalSpec.absentPatterns
                        |> Maybe.map List.Nonempty.toList
                        |> Maybe.withDefault []
                    )
                        ++ getImpliedAbsentPatterns nonFinalSpec
                        |> List.Extra.unique
                        |> List.Nonempty.fromList
            }
    in
    List.all
        identity
        [ case finalSpec.patterns of
            Nothing ->
                True

            Just patterns ->
                patterns
                    |> List.Nonempty.map
                        (getPatternStickers
                            >> List.Nonempty.map (getStickerColor stickers)
                            >> List.Nonempty.uniq
                        )
                    -- Every pattern should have all the colors be the same
                    |> List.Nonempty.all (List.Nonempty.length >> (==) 1)
        , case finalSpec.absentPatterns of
            Nothing ->
                True

            Just absentPatterns ->
                absentPatterns
                    |> List.Nonempty.map
                        (getPatternStickers
                            >> List.Nonempty.map (getStickerColor stickers)
                            >> List.Nonempty.uniq
                        )
                    -- Every pattern should have at least two different colors as it should
                    -- be an absent pattern
                    |> List.Nonempty.all (\list -> List.Nonempty.length list > 1)
        , case finalSpec.oppositelyColored of
            Nothing ->
                True

            Just oppositelyColored ->
                oppositelyColored
                    |> mapSameForBoth
                        (List.Nonempty.concatMap getElementStickers
                            >> List.Nonempty.map (getStickerColor stickers)
                            >> List.Nonempty.uniq
                        )
                    |> (\x ->
                            case x of
                                -- They have to each group be the same color all stickers
                                -- and for that color to be opposite to the one of the
                                -- other group
                                ( List.Nonempty.Nonempty firstColor [], List.Nonempty.Nonempty secondColor [] ) ->
                                    areOppositeColors firstColor secondColor

                                _ ->
                                    False
                       )
        , case finalSpec.adjacentlyColored of
            Nothing ->
                True

            Just adjacentlyColored ->
                adjacentlyColored
                    |> mapSameForBoth
                        (List.Nonempty.concatMap getElementStickers
                            >> List.Nonempty.map (getStickerColor stickers)
                            >> List.Nonempty.uniq
                        )
                    |> (\x ->
                            case x of
                                -- They have to each group be the same color all stickers
                                -- and for that color to be adjacent to the one of the
                                -- other group
                                ( List.Nonempty.Nonempty firstColor [], List.Nonempty.Nonempty secondColor [] ) ->
                                    areAdjacentColors firstColor secondColor

                                _ ->
                                    False
                       )
        , case finalSpec.identicallyColored of
            Nothing ->
                True

            Just ( first, second, tail ) ->
                List.Nonempty.Nonempty first (second :: tail)
                    |> List.Nonempty.concatMap getElementStickers
                    |> List.Nonempty.map (getStickerColor stickers)
                    |> List.Nonempty.uniq
                    |> (List.Nonempty.length >> (==) 1)
        , case finalSpec.differentlyColored of
            Nothing ->
                True

            Just ( first, second, tail ) ->
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
        , case finalSpec.noOtherStickersMatchThanThese of
            Nothing ->
                True

            Just elements ->
                let
                    allGroupsAreSameColored =
                        elements
                            |> List.Nonempty.map
                                (getElementStickers
                                    >> List.Nonempty.map (getStickerColor stickers)
                                    >> List.Nonempty.uniq
                                    >> List.Nonempty.length
                                )
                            |> List.Nonempty.all ((==) 1)

                    excludedStickers =
                        elements
                            |> List.Nonempty.concatMap getElementStickers

                    excludedColors =
                        excludedStickers
                            |> List.Nonempty.map (getStickerColor stickers)
                            |> List.Nonempty.uniq

                    includedStickers =
                        allStickers
                            |> List.Nonempty.toList
                            |> List.filter
                                (\x ->
                                    not <| List.Nonempty.member x excludedStickers
                                )

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
                allGroupsAreSameColored && noExcludedColorsAreMatched && allStickersHaveADistinctColor
        ]


getImpliedAbsentPatterns : PLL.RecognitionSpecification -> List PLL.RecognitionPattern
getImpliedAbsentPatterns spec =
    spec.patterns
        |> Maybe.map List.Nonempty.toList
        |> Maybe.withDefault []
        |> List.filterMap
            (\pattern ->
                case pattern of
                    -- We don't need to specify for example left inside / outside two bar
                    -- as those being there at the same time to headlights are equivalent
                    -- to the three bar
                    PLL.LeftHeadlights ->
                        Just LeftThreeBar

                    PLL.RightHeadlights ->
                        Just RightThreeBar

                    PLL.LeftInsideTwoBar ->
                        Just LeftThreeBar

                    PLL.RightInsideTwoBar ->
                        Just RightThreeBar

                    PLL.LeftOutsideTwoBar ->
                        Just LeftThreeBar

                    PLL.RightOutsideTwoBar ->
                        Just RightThreeBar

                    PLL.LeftThreeBar ->
                        Nothing

                    PLL.RightThreeBar ->
                        Nothing

                    PLL.Bookends ->
                        Nothing
            )


mapSameForBoth : (a -> b) -> ( a, a ) -> ( b, b )
mapSameForBoth f ( first, second ) =
    ( f first, f second )


allStickers : List.Nonempty.Nonempty PLL.Sticker
allStickers =
    List.Nonempty.Nonempty
        PLL.FirstStickerFromLeft
        [ SecondStickerFromLeft
        , ThirdStickerFromLeft
        , ThirdStickerFromRight
        , SecondStickerFromRight
        , FirstStickerFromRight
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
        extractPatternsFromMaybePatterns spec.patterns
            ++ extractPatternsFromMaybePatterns spec.absentPatterns
            ++ extractPatternsFromTuple spec.oppositelyColored
            ++ extractPatternsFromTuple spec.adjacentlyColored
            ++ extractPatternsFromMinLength2List spec.identicallyColored
            ++ extractPatternsFromMinLength2List spec.differentlyColored
            ++ extractPatternsFromMaybeElements spec.noOtherStickersMatchThanThese


extractPatternsFromMaybePatterns : Maybe (List.Nonempty.Nonempty PLL.RecognitionPattern) -> List PLL.RecognitionPattern
extractPatternsFromMaybePatterns maybeList =
    maybeList
        |> Maybe.map List.Nonempty.toList
        |> Maybe.withDefault []


extractPatternsFromTuple : Maybe ( List.Nonempty.Nonempty PLL.RecognitionElement, List.Nonempty.Nonempty PLL.RecognitionElement ) -> List PLL.RecognitionPattern
extractPatternsFromTuple maybeTuple =
    maybeTuple
        |> Maybe.map (\( a, b ) -> List.Nonempty.toList a ++ List.Nonempty.toList b)
        |> Maybe.withDefault []
        |> List.filterMap getPattern


extractPatternsFromMinLength2List : Maybe ( PLL.RecognitionElement, PLL.RecognitionElement, List PLL.RecognitionElement ) -> List PLL.RecognitionPattern
extractPatternsFromMinLength2List maybeList =
    maybeList
        |> Maybe.map (\( first, second, tail ) -> first :: second :: tail)
        |> Maybe.withDefault []
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
