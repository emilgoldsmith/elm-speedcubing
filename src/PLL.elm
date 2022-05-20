module PLL exposing
    ( PLL(..), all
    , getLetters, solvedBy, getAllEquivalentAUFs
    , Algorithms, getAlgorithm, referenceAlgorithms
    )

{-| Types and helper functions to work with the Permutate Last
Layer (PLL) algorithm set, the last step of the CFOP method. See
<https://www.speedsolving.com/wiki/index.php/PLL>
for further information


# Definition And Constructors

@docs PLL, all


# Helpers

@docs getLetters, solvedBy, getAllEquivalentAUFs


# Collections

@docs Algorithms, getAlgorithm, referenceAlgorithms

-}

import AUF exposing (AUF)
import Algorithm exposing (Algorithm)
import Cube
import List.Nonempty
import Utils.Enumerator



-- DEFINITION


{-| All the cases are represented here. Use the value constructors here
or [@all](#all) to specify a given case in your code, and pass these to
any of the helper functions
-}
type PLL
    = -- Edges only
      H
    | Ua
    | Ub
    | Z
      -- Corners only
    | Aa
    | Ab
    | E
      -- Edges And Corners
    | F
    | Ga
    | Gb
    | Gc
    | Gd
    | Ja
    | Jb
    | Na
    | Nb
    | Ra
    | Rb
    | T
    | V
    | Y


{-| A non-empty list of all the PLLs. Can for example
be used for randomly selecting a pll

    import List.Nonempty

    -- All the PLLs are there!
    List.Nonempty.length all --> 21

    -- We could for example select a random PLL case
    -- via this
    List.Nonempty.sample all

-}
all : List.Nonempty.Nonempty PLL
all =
    let
        fromH pll =
            case pll of
                H ->
                    Just Ua

                Ua ->
                    Just Ub

                Ub ->
                    Just Z

                Z ->
                    Just Aa

                Aa ->
                    Just Ab

                Ab ->
                    Just E

                E ->
                    Just F

                F ->
                    Just Ga

                Ga ->
                    Just Gb

                Gb ->
                    Just Gc

                Gc ->
                    Just Gd

                Gd ->
                    Just Ja

                Ja ->
                    Just Jb

                Jb ->
                    Just Na

                Na ->
                    Just Nb

                Nb ->
                    Just Ra

                Ra ->
                    Just Rb

                Rb ->
                    Just T

                T ->
                    Just V

                V ->
                    Just Y

                Y ->
                    Nothing
    in
    Utils.Enumerator.from H fromH



-- HELPERS


{-| Generates a string of the identifying letters of the case.

This could be used either for serialization purposes, or for
building a string to display the user in some instances.

    -- Format is always first letter capitalized and
    -- the second one lower case if applicable
    getLetters Ua --> "Ua"

    getLetters H --> "H"

-}
getLetters : PLL -> String
getLetters pll =
    case pll of
        H ->
            "H"

        Ua ->
            "Ua"

        Ub ->
            "Ub"

        Z ->
            "Z"

        Aa ->
            "Aa"

        Ab ->
            "Ab"

        E ->
            "E"

        F ->
            "F"

        Ga ->
            "Ga"

        Gb ->
            "Gb"

        Gc ->
            "Gc"

        Gd ->
            "Gd"

        Ja ->
            "Ja"

        Jb ->
            "Jb"

        Na ->
            "Na"

        Nb ->
            "Nb"

        Ra ->
            "Ra"

        Rb ->
            "Rb"

        T ->
            "T"

        V ->
            "V"

        Y ->
            "Y"


{-| Check whether an algorithm solves a PLL case.

Note that actually solving the cube depends on it being in
the correct execution angle as well and doing the last AUF.
This function just checks if with those correctly aligned
the algorithm can solve it. So different algorithms can
pass this as seen in these examples:

    import Algorithm

    Algorithm.fromString "(x) R' U R' D2 R U' R' D2 R2 (x')"
        |> Result.map (\alg -> solvedBy alg Aa)
    --> Ok True

    Algorithm.fromString "U (x) R' U R' D2 R U' R' D2 R2 (x')"
        |> Result.map (\alg -> solvedBy alg Aa)
    --> Ok True

    Algorithm.fromString "(x) R' U R' D2 R U' R' D2 R2 (x') U"
        |> Result.map (\alg -> solvedBy alg Aa)
    --> Ok True

    Algorithm.fromString "U"
        |> Result.map (\alg -> solvedBy alg Aa)
    --> Ok False

-}
solvedBy : Algorithm -> PLL -> Bool
solvedBy algorithm pll =
    let
        allAufCombinations =
            List.Nonempty.concatMap
                (\preAUF -> List.Nonempty.map (Tuple.pair preAUF) AUF.all)
                AUF.all
    in
    allAufCombinations
        |> List.Nonempty.any
            (\aufs ->
                AUF.addToAlgorithm aufs algorithm
                    |> Cube.algorithmResultsAreEquivalentIndependentOfFinalRotation
                        (getAlgorithm referenceAlgorithms pll)
            )


{-| Calculates and returns all pairs of AUFs that are equivalent to the given pair of
AUFs for the given PLL
-}
getAllEquivalentAUFs : ( AUF, PLL, AUF ) -> List.Nonempty.Nonempty ( AUF, AUF )
getAllEquivalentAUFs ( preAUF, pll, postAUF ) =
    case getSymmetry pll of
        NotSymmetric ->
            List.Nonempty.singleton ( preAUF, postAUF )

        HalfSymmetric ->
            List.Nonempty.Nonempty
                ( preAUF, postAUF )
                [ ( AUF.add preAUF AUF.Halfway, AUF.add postAUF AUF.Halfway ) ]

        NPermSymmetric ->
            AUF.all
                |> List.Nonempty.map
                    (\toAdd ->
                        ( AUF.add preAUF toAdd, AUF.add postAUF toAdd )
                    )

        FullySymmetric ->
            let
                allAUFPairs =
                    AUF.all
                        |> List.Nonempty.concatMap
                            (\preAUF_ ->
                                List.Nonempty.map (Tuple.pair preAUF_) AUF.all
                            )
            in
            allAUFPairs
                |> List.Nonempty.filter
                    (\candidate ->
                        AUF.add preAUF postAUF == AUF.add (Tuple.first candidate) (Tuple.second candidate)
                    )
                    -- Should never be necessary as this case itself should also
                    -- be contained in the list of all AUF pairs
                    ( preAUF, postAUF )


type PLLSymmetry
    = NotSymmetric
    | HalfSymmetric
    | NPermSymmetric
    | FullySymmetric


getSymmetry : PLL -> PLLSymmetry
getSymmetry pll =
    case pll of
        H ->
            FullySymmetric

        Ua ->
            NotSymmetric

        Ub ->
            NotSymmetric

        Z ->
            HalfSymmetric

        Aa ->
            NotSymmetric

        Ab ->
            NotSymmetric

        E ->
            HalfSymmetric

        F ->
            NotSymmetric

        Ga ->
            NotSymmetric

        Gb ->
            NotSymmetric

        Gc ->
            NotSymmetric

        Gd ->
            NotSymmetric

        Ja ->
            NotSymmetric

        Jb ->
            NotSymmetric

        Na ->
            NPermSymmetric

        Nb ->
            NPermSymmetric

        Ra ->
            NotSymmetric

        Rb ->
            NotSymmetric

        T ->
            NotSymmetric

        V ->
            NotSymmetric

        Y ->
            NotSymmetric



-- COLLECTIONS


{-| A collection of algorithms that solves the respective case
-}
type alias Algorithms =
    { -- Edges only
      h : Algorithm
    , ua : Algorithm
    , ub : Algorithm
    , z : Algorithm

    -- Corners only
    , aa : Algorithm
    , ab : Algorithm
    , e : Algorithm

    -- Edges And Corners
    , f : Algorithm
    , ga : Algorithm
    , gb : Algorithm
    , gc : Algorithm
    , gd : Algorithm
    , ja : Algorithm
    , jb : Algorithm
    , na : Algorithm
    , nb : Algorithm
    , ra : Algorithm
    , rb : Algorithm
    , t : Algorithm
    , v : Algorithm
    , y : Algorithm
    }


{-| Get the algorithm for the PLL case from an algorithm collection.
This helps avoid any typos or need to write your own case statements
in order to get the algorithm for a case passed to a function

    getAlgorithm referenceAlgorithms Y
    --> referenceAlgorithms.y

-}
getAlgorithm : Algorithms -> PLL -> Algorithm
getAlgorithm algorithms pll =
    case pll of
        H ->
            algorithms.h

        Ua ->
            algorithms.ua

        Ub ->
            algorithms.ub

        Z ->
            algorithms.z

        Aa ->
            algorithms.aa

        Ab ->
            algorithms.ab

        E ->
            algorithms.e

        F ->
            algorithms.f

        Ga ->
            algorithms.ga

        Gb ->
            algorithms.gb

        Gc ->
            algorithms.gc

        Gd ->
            algorithms.gd

        Ja ->
            algorithms.ja

        Jb ->
            algorithms.jb

        Na ->
            algorithms.na

        Nb ->
            algorithms.nb

        Ra ->
            algorithms.ra

        Rb ->
            algorithms.rb

        T ->
            algorithms.t

        V ->
            algorithms.v

        Y ->
            algorithms.y


{-| Plls verified to be correct so they can be used to verify user selected plls
or for displaying a pll case somewhere on the site.

They have been chosen to be the optimally lowest move count in HTM just for a
small performance boost.

The example tests below are just meant for an easier
to read version of all the algorithms that are verified to be correct.
They are also tested via elm-verify-examples so the string versions are
correct equivalents to the code below.

    import Algorithm

    -- Edges Only

    Algorithm.fromString "R2 U2 R U2 R2 U2 R2 U2 R U2 R2"
    --> Ok referenceAlgorithms.h

    Algorithm.fromString "F2 U' (L R') F2 (L' R) U' F2"
    --> Ok referenceAlgorithms.ua

    Algorithm.fromString "F2 U (R' L) F2 (R L') U F2"
    --> Ok referenceAlgorithms.ub

    Algorithm.fromString "R B' R' B F R' F B' R' B R F2"
    --> Ok referenceAlgorithms.z

    -- Corners Only

    Algorithm.fromString "R' F R' B2 R F' R' B2 R2"
    --> Ok referenceAlgorithms.aa

    Algorithm.fromString "R B' R F2 R' B R F2 R2"
    --> Ok referenceAlgorithms.ab

    Algorithm.fromString "D R' D2 F' D L D' F D2 R D' F' L' F"
    --> Ok referenceAlgorithms.e

    -- Corners And Edges

    Algorithm.fromString "L F R' F' L' F' D2 B' L' B D2 F' R F2"
    --> Ok referenceAlgorithms.f

    Algorithm.fromString "F2' D (R' U R' U' R) D' F2 L' U L"
    --> Ok referenceAlgorithms.ga

    Algorithm.fromString "R' U' R B2 D (L' U L U' L) D' B2"
    --> Ok referenceAlgorithms.gb

    Algorithm.fromString "R2' D' F U' F U F' D R2 B U' B'"
    --> Ok referenceAlgorithms.gc

    Algorithm.fromString "R U R' F2 D' (L U' L' U L') D F2"
    --> Ok referenceAlgorithms.gd

    Algorithm.fromString "B2 R' U' R B2 L' D L' D' L2"
    --> Ok referenceAlgorithms.ja

    Algorithm.fromString "B2 (L U L') B2 (R D' R D) R2"
    --> Ok referenceAlgorithms.jb

    Algorithm.fromString "L U' R U2 L' U R' L U' R U2 L' U R'"
    --> Ok referenceAlgorithms.na

    Algorithm.fromString "R' U L' U2 R U' L R' U L' U2 R U' L"
    --> Ok referenceAlgorithms.nb

    Algorithm.fromString "F2 R' F' U' F' U F R F' U2 F U2 F'"
    --> Ok referenceAlgorithms.ra

    Algorithm.fromString "R2 F R U R U' R' F' R U2 R' U2 R"
    --> Ok referenceAlgorithms.rb

    Algorithm.fromString "F2 D R2 U' R2 F2 D' L2 U L2"
    --> Ok referenceAlgorithms.t

    Algorithm.fromString "R' U R' U' B' R' B2 U' B' U B' R B R"
    --> Ok referenceAlgorithms.v

    Algorithm.fromString "F2 D R2 U R2 D' R' U' R F2 R' U R"
    --> Ok referenceAlgorithms.y

-}
referenceAlgorithms : Algorithms
referenceAlgorithms =
    { h =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            ]
    , ua =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            ]
    , ub =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            ]
    , z =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            ]
    , aa =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            ]
    , ab =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            ]
    , e =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            ]
    , f =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            ]
    , ga =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            ]
    , gb =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            ]
    , gc =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
            ]
    , gd =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            ]
    , ja =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.Halfway Algorithm.Clockwise
            ]
    , jb =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            ]
    , na =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            ]
    , nb =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
            ]
    , ra =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            ]
    , rb =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            ]
    , t =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.L Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.L Algorithm.Halfway Algorithm.Clockwise
            ]
    , v =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            ]
    , y =
        Algorithm.fromTurnList
            [ Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.F Algorithm.Halfway Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
            ]
    }
