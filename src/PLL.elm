module PLL exposing
    ( PLL(..), all
    , getLetters, solvedBy, getAllEquivalentAUFs, getAllAUFEquivalencyClasses
    , Algorithms, getAlgorithm, referenceAlgorithms
    , RecognitionAngle, RecognitionElement(..), RecognitionError(..), RecognitionPattern(..), RecognitionSpecification, Sticker(..), getUniqueTwoSidedRecognitionSpecification, uflRecognitionAngle, ufrRecognitionAngle
    )

{-| Types and helper functions to work with the Permutate Last
Layer (PLL) algorithm set, the last step of the CFOP method. See
<https://www.speedsolving.com/wiki/index.php/PLL>
for further information


# Definition And Constructors

@docs PLL, all


# Helpers

@docs getLetters, solvedBy, getAllEquivalentAUFs, getAllAUFEquivalencyClasses


# Collections

@docs Algorithms, getAlgorithm, referenceAlgorithms

-}

import AUF exposing (AUF)
import Algorithm exposing (Algorithm)
import Cube
import Cube.Advanced
import List.Extra
import List.Nonempty
import List.Nonempty.Extra
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
        preAUFsThatNeedChecking =
            -- We don't always need to check every preAUF as there are several
            -- PLLs that can be executed from several different angles
            -- so we just need to make sure we've covered "all bases"
            case getSymmetry pll of
                NotSymmetric ->
                    AUF.all

                HalfSymmetric ->
                    List.Nonempty.Nonempty AUF.None [ AUF.Clockwise ]

                NPermSymmetric ->
                    List.Nonempty.singleton AUF.None

                FullySymmetric ->
                    List.Nonempty.singleton AUF.None

        -- We reimplement some algorithm equivalency algorithms here to save on some recomputation
        -- and optimize the runtime
        cubeAlgorithmShouldSolve =
            Cube.applyAlgorithm
                (Algorithm.inverse <|
                    Cube.makeAlgorithmMaintainOrientation <|
                        getAlgorithm referenceAlgorithms pll
                )
                Cube.solved

        algorithmWithMaintainedOrientation =
            Cube.makeAlgorithmMaintainOrientation algorithm
    in
    preAUFsThatNeedChecking
        |> List.Nonempty.any
            (\preAUF ->
                cubeAlgorithmShouldSolve
                    |> Cube.applyAlgorithm (AUF.toAlgorithm preAUF)
                    |> Cube.applyAlgorithm algorithmWithMaintainedOrientation
                    |> Cube.Advanced.canBeSolvedBySingleUTurn
            )


{-| Calculates and returns all pairs of AUFs that are equivalent to the given pair of
AUFs for the given PLL

    import AUF
    import Expect
    import Expect.Extra exposing (equalNonEmptyListMembers)
    import List.Nonempty

    getAllEquivalentAUFs ( AUF.None, PLL.H, AUF.None )
        |> equalNonEmptyListMembers
            (List.Nonempty.Nonempty
                ( AUF.None, AUF.None )
                [ ( AUF.Clockwise, AUF.CounterClockwise )
                , ( AUF.Halfway, AUF.Halfway )
                , ( AUF.CounterClockwise, AUF.Clockwise )
                ]
            )
    --> Expect.pass

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
            AUF.all
                |> List.Nonempty.map
                    (\toAdd ->
                        let
                            inverseOfToAdd =
                                toAdd
                                    |> AUF.toAlgorithm
                                    |> Algorithm.inverse
                                    |> AUF.fromAlgorithm
                                    -- This case should never occur but otherwise unit tests should catch it
                                    |> Maybe.withDefault AUF.None
                        in
                        ( AUF.add preAUF toAdd, AUF.add postAUF inverseOfToAdd )
                    )


{-| Returns a list of lists, where each sublist represents an equivalency class of
AUF pairs for the given pll. The equivalency classes are also exhaustive, so every
possible AUF pair is represented in this list of lists
-}
getAllAUFEquivalencyClasses : PLL -> List.Nonempty.Nonempty (List.Nonempty.Nonempty ( AUF, AUF ))
getAllAUFEquivalencyClasses pll =
    let
        allAUFPairs =
            List.Nonempty.Extra.lift2 Tuple.pair
                AUF.all
                AUF.all
    in
    allAUFPairs
        |> List.Nonempty.foldl
            (\aufPair equivalencyClasses ->
                if List.Nonempty.member aufPair (List.Nonempty.concat equivalencyClasses) then
                    equivalencyClasses

                else
                    List.Nonempty.cons (getAllEquivalentAUFs ( Tuple.first aufPair, pll, Tuple.second aufPair )) equivalencyClasses
            )
            (List.Nonempty.singleton
                (getAllEquivalentAUFs ( AUF.None, pll, AUF.None ))
            )


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


type alias RecognitionSpecification =
    { caseRecognition : CaseRecognitionSpecification
    , postAUFRecognition :
        List.Nonempty.Nonempty
            ( List.Nonempty.Nonempty RecognitionElement
            , Cube.Advanced.Face
            )
    }


type alias CaseRecognitionSpecification =
    { patterns : Maybe (List.Nonempty.Nonempty RecognitionPattern)
    , absentPatterns : Maybe (List.Nonempty.Nonempty RecognitionPattern)
    , oppositelyColored : List ( List.Nonempty.Nonempty RecognitionElement, List.Nonempty.Nonempty RecognitionElement )
    , notOppositelyColored : List ( List.Nonempty.Nonempty RecognitionElement, List.Nonempty.Nonempty RecognitionElement )
    , adjacentlyColored : List ( List.Nonempty.Nonempty RecognitionElement, List.Nonempty.Nonempty RecognitionElement )
    , identicallyColored : Maybe ( RecognitionElement, RecognitionElement, List RecognitionElement )
    , differentlyColored : Maybe ( RecognitionElement, RecognitionElement, List RecognitionElement )
    , noOtherStickersMatchThanThese : Maybe (List.Nonempty.Nonempty RecognitionElement)
    , noOtherBlocksPresent : Bool
    }


emptyCaseSpec : CaseRecognitionSpecification
emptyCaseSpec =
    { patterns = Nothing
    , absentPatterns = Nothing
    , oppositelyColored = []
    , notOppositelyColored = []
    , adjacentlyColored = []
    , identicallyColored = Nothing
    , differentlyColored = Nothing
    , noOtherStickersMatchThanThese = Nothing
    , noOtherBlocksPresent = False
    }


type RecognitionElement
    = Pattern RecognitionPattern
    | Sticker Sticker


isPattern : RecognitionElement -> Bool
isPattern element =
    case element of
        Pattern _ ->
            True

        Sticker _ ->
            False


type RecognitionPattern
    = LeftHeadlights
    | RightHeadlights
    | LeftThreeBar
    | RightThreeBar
    | LeftInsideTwoBar
    | RightInsideTwoBar
    | LeftOutsideTwoBar
    | RightOutsideTwoBar
    | Bookends
    | LeftFourChecker
    | RightFourChecker
    | InnerFourChecker
    | LeftFiveChecker
    | RightFiveChecker
    | SixChecker


type Sticker
    = FirstStickerFromLeft
    | SecondStickerFromLeft
    | ThirdStickerFromLeft
    | FirstStickerFromRight
    | SecondStickerFromRight
    | ThirdStickerFromRight


allStickers : List.Nonempty.Nonempty Sticker
allStickers =
    List.Nonempty.Nonempty
        FirstStickerFromLeft
        [ SecondStickerFromLeft
        , ThirdStickerFromLeft
        , ThirdStickerFromRight
        , SecondStickerFromRight
        , FirstStickerFromRight
        ]


type RecognitionAngle
    = UFR
    | UFL


ufrRecognitionAngle : RecognitionAngle
ufrRecognitionAngle =
    UFR


uflRecognitionAngle : RecognitionAngle
uflRecognitionAngle =
    UFL


type RecognitionError
    = IncorrectPLLAlgorithm PLL Algorithm


getUniqueTwoSidedRecognitionSpecification :
    Algorithms
    -> RecognitionAngle
    -> ( AUF, PLL )
    -> Result RecognitionError RecognitionSpecification
getUniqueTwoSidedRecognitionSpecification algorithms recognitionAngle ( originalPreAUF, pll ) =
    let
        toAddForReferenceAlgorithm =
            Cube.detectAUFs
                { toDetectFor = getAlgorithm referenceAlgorithms pll
                , toMatchTo = getAlgorithm algorithms pll
                }
                |> Maybe.map Tuple.first
                |> Result.fromMaybe
                    (IncorrectPLLAlgorithm pll (getAlgorithm algorithms pll))

        toAddForUFRAngle =
            case recognitionAngle of
                UFR ->
                    AUF.None

                UFL ->
                    AUF.Clockwise

        ufrReferenceAlgorithmPreAUFResult =
            toAddForReferenceAlgorithm
                |> Result.map (AUF.add toAddForUFRAngle)
                |> Result.map (AUF.add originalPreAUF)

        caseSpecResult =
            ufrReferenceAlgorithmPreAUFResult
                |> Result.map
                    (\ufrReferenceAlgorithmPreAUF ->
                        getUfrReferenceAlgorithmCaseSpec ufrReferenceAlgorithmPreAUF pll
                    )
    in
    caseSpecResult
        |> Result.map
            (\caseSpec ->
                { caseRecognition = caseSpec
                , postAUFRecognition =
                    let
                        caseRendering =
                            Cube.solved
                                |> Cube.applyAlgorithm
                                    (Algorithm.inverse <|
                                        Algorithm.append (AUF.toAlgorithm originalPreAUF) <|
                                            getAlgorithm algorithms pll
                                    )
                                |> Cube.Advanced.render

                        allRelevantElements =
                            caseSpec.patterns
                                |> Maybe.map
                                    (List.Nonempty.map Pattern
                                        >> List.Nonempty.append
                                            (List.Nonempty.map Sticker allStickers)
                                    )
                                |> Maybe.withDefault (List.Nonempty.map Sticker allStickers)

                        elementsWithTargets =
                            allRelevantElements
                                |> List.Nonempty.toList
                                |> List.filterMap
                                    (getOriginalAndTargetFace
                                        caseRendering
                                        recognitionAngle
                                    )

                        patternsThatStayInPlace =
                            elementsWithTargets
                                |> List.filterMap
                                    (\{ element, originalFace, finalFace } ->
                                        case element of
                                            Sticker _ ->
                                                Nothing

                                            Pattern pattern ->
                                                if originalFace == finalFace then
                                                    Just ( pattern, finalFace )

                                                else
                                                    Nothing
                                    )
                    in
                    -- If a pattern stays in place we want the most visible of those
                    -- as this is obviously the easiest to recognize post AUF with
                    case
                        List.head <|
                            List.sortBy (Tuple.first >> howVisibleIsPattern) <|
                                patternsThatStayInPlace
                    of
                        Just patternThatStaysInPlace ->
                            List.Nonempty.singleton <|
                                Tuple.mapFirst (\element -> List.Nonempty.singleton (Pattern element)) <|
                                    patternThatStaysInPlace

                        Nothing ->
                            let
                                groupedElements =
                                    elementsWithTargets
                                        |> List.foldl
                                            (\{ element, originalFace, finalFace } acc ->
                                                if originalFace == finalFace then
                                                    { acc
                                                        | staysInPlace = ( element, finalFace ) :: acc.staysInPlace
                                                    }

                                                else if faceVisible recognitionAngle finalFace then
                                                    { acc
                                                        | switchesToOtherVisibleSide =
                                                            ( element, finalFace )
                                                                :: acc.switchesToOtherVisibleSide
                                                    }

                                                else
                                                    { acc
                                                        | rest = ( element, finalFace ) :: acc.rest
                                                    }
                                            )
                                            { staysInPlace = []
                                            , switchesToOtherVisibleSide = []
                                            , rest = []
                                            }

                                stickersAlsoGrouped =
                                    { staysInPlace =
                                        groupStickersByFinalFace groupedElements.staysInPlace
                                    , switchesToOtherVisibleSide =
                                        groupStickersByFinalFace
                                            groupedElements.switchesToOtherVisibleSide
                                    , rest = groupStickersByFinalFace groupedElements.rest
                                    }

                                sortedGroupedElements =
                                    { staysInPlace =
                                        List.sortBy (Tuple.first >> howVisibleIsElementGroup)
                                            stickersAlsoGrouped.staysInPlace
                                    , switchesToOtherVisibleSide =
                                        List.sortBy (Tuple.first >> howVisibleIsElementGroup)
                                            stickersAlsoGrouped.switchesToOtherVisibleSide
                                    , rest =
                                        List.sortBy (Tuple.first >> howVisibleIsElementGroup)
                                            stickersAlsoGrouped.rest
                                    }

                                bestPatternsThatStayVisible =
                                    List.filterMap identity
                                        [ List.head sortedGroupedElements.staysInPlace
                                        , List.head sortedGroupedElements.switchesToOtherVisibleSide
                                        ]

                                finalList =
                                    List.sortBy (Tuple.first >> howVisibleIsElementGroup)
                                        (if
                                            bestPatternsThatStayVisible
                                                |> List.any
                                                    (Tuple.first
                                                        >> List.Nonempty.Extra.find isPattern
                                                        >> (/=) Nothing
                                                    )
                                         then
                                            bestPatternsThatStayVisible

                                         else
                                            List.concat
                                                [ bestPatternsThatStayVisible
                                                , List.head sortedGroupedElements.rest
                                                    |> Maybe.map List.singleton
                                                    |> Maybe.withDefault []
                                                ]
                                        )
                            in
                            finalList
                                |> List.Nonempty.fromList
                                |> Maybe.withDefault
                                    -- This should never happen so make a nonsense case that should
                                    -- definitely fail tests and obviously be wrong
                                    (List.Nonempty.Nonempty
                                        ( List.Nonempty.singleton <| Pattern LeftThreeBar
                                        , Cube.Advanced.FrontOrBack Cube.Advanced.F
                                        )
                                        [ ( List.Nonempty.singleton <| Pattern SixChecker
                                          , Cube.Advanced.FrontOrBack Cube.Advanced.F
                                          )
                                        ]
                                    )
                }
            )


groupStickersByFinalFace : List ( RecognitionElement, Cube.Advanced.Face ) -> List ( List.Nonempty.Nonempty RecognitionElement, Cube.Advanced.Face )
groupStickersByFinalFace list =
    let
        groupedByFace =
            list
                |> List.foldl
                    (\( element, face ) acc ->
                        -- The order doesn't matter just that they get separated
                        case face of
                            Cube.Advanced.FrontOrBack Cube.Advanced.F ->
                                { acc
                                    | f = element :: acc.f
                                }

                            Cube.Advanced.FrontOrBack Cube.Advanced.B ->
                                { acc
                                    | b = element :: acc.b
                                }

                            Cube.Advanced.LeftOrRight Cube.Advanced.L ->
                                { acc
                                    | l = element :: acc.l
                                }

                            Cube.Advanced.LeftOrRight Cube.Advanced.R ->
                                { acc
                                    | r = element :: acc.r
                                }

                            Cube.Advanced.UpOrDown Cube.Advanced.U ->
                                { acc
                                    | u = element :: acc.u
                                }

                            Cube.Advanced.UpOrDown Cube.Advanced.D ->
                                { acc
                                    | d = element :: acc.d
                                }
                    )
                    { u = []
                    , d = []
                    , l = []
                    , r = []
                    , f = []
                    , b = []
                    }
    in
    [ ( groupedByFace.u, Cube.Advanced.UpOrDown Cube.Advanced.U )
    , ( groupedByFace.d, Cube.Advanced.UpOrDown Cube.Advanced.D )
    , ( groupedByFace.l, Cube.Advanced.LeftOrRight Cube.Advanced.L )
    , ( groupedByFace.r, Cube.Advanced.LeftOrRight Cube.Advanced.R )
    , ( groupedByFace.f, Cube.Advanced.FrontOrBack Cube.Advanced.F )
    , ( groupedByFace.b, Cube.Advanced.FrontOrBack Cube.Advanced.B )
    ]
        |> List.foldl
            (\( elements, face ) acc ->
                case List.Nonempty.fromList elements of
                    Nothing ->
                        acc

                    Just nonemptyElements ->
                        ( nonemptyElements, face ) :: acc
            )
            []


faceVisible : RecognitionAngle -> Cube.Advanced.Face -> Bool
faceVisible angle face =
    case angle of
        UFR ->
            case face of
                Cube.Advanced.FrontOrBack Cube.Advanced.F ->
                    True

                Cube.Advanced.LeftOrRight Cube.Advanced.R ->
                    True

                _ ->
                    False

        UFL ->
            case face of
                Cube.Advanced.FrontOrBack Cube.Advanced.F ->
                    True

                Cube.Advanced.LeftOrRight Cube.Advanced.L ->
                    True

                _ ->
                    False


{-| Lower is more visible
-}
howVisibleIsElementGroup : List.Nonempty.Nonempty RecognitionElement -> Float
howVisibleIsElementGroup elements =
    elements
        |> List.Nonempty.map
            (\element ->
                case element of
                    Sticker _ ->
                        -- Just an excessively high value as stickers should
                        -- all be worth the same and be sorted after patterns
                        -- , also sorted after patterns even
                        -- when several of them later are grouped together
                        -- which makes them more visible
                        1000

                    Pattern pattern ->
                        howVisibleIsPattern pattern
            )
        |> List.Nonempty.Extra.minimum
        -- Sort groups earlier if there are for example more stickers with
        -- same color
        |> (\x -> x / (toFloat <| List.Nonempty.length elements))


{-| Lower is more visible
-}
howVisibleIsPattern : RecognitionPattern -> Float
howVisibleIsPattern pattern =
    case pattern of
        LeftThreeBar ->
            1

        RightThreeBar ->
            1

        LeftInsideTwoBar ->
            2

        RightInsideTwoBar ->
            2

        LeftOutsideTwoBar ->
            2

        RightOutsideTwoBar ->
            2

        LeftHeadlights ->
            3

        RightHeadlights ->
            3

        -- Not that much effort is put in putting good values down here
        -- as at the time of writing it's actually not utilized
        Bookends ->
            4

        SixChecker ->
            5

        LeftFiveChecker ->
            6

        RightFiveChecker ->
            6

        LeftFourChecker ->
            7

        RightFourChecker ->
            7

        InnerFourChecker ->
            7


getOriginalAndTargetFace :
    Cube.Advanced.Rendering
    -> RecognitionAngle
    -> RecognitionElement
    -> Maybe { element : RecognitionElement, originalFace : Cube.Advanced.Face, finalFace : Cube.Advanced.Face }
getOriginalAndTargetFace noPostAUFCaseRendering angle element =
    let
        representativeSticker =
            case element of
                Sticker sticker ->
                    Just sticker

                Pattern pattern ->
                    -- By choosing a representative sticker here we assume that
                    -- the pattern given in the element is correctly present, and
                    -- we filter out any patterns that are multicolored or
                    -- spanning several faces, as they aren't as helpful for
                    -- postAUF recognition and can just as well be identified
                    -- just by single stickers. Within these parameters we can
                    -- just use a single sticker and identify that stickers original
                    -- and final face and it should apply for the whole pattern.
                    case pattern of
                        LeftFourChecker ->
                            Nothing

                        RightFourChecker ->
                            Nothing

                        InnerFourChecker ->
                            Nothing

                        LeftFiveChecker ->
                            Nothing

                        RightFiveChecker ->
                            Nothing

                        SixChecker ->
                            Nothing

                        Bookends ->
                            Nothing

                        LeftHeadlights ->
                            Just FirstStickerFromLeft

                        RightHeadlights ->
                            Just FirstStickerFromRight

                        LeftThreeBar ->
                            Just FirstStickerFromLeft

                        RightThreeBar ->
                            Just FirstStickerFromRight

                        LeftInsideTwoBar ->
                            Just SecondStickerFromLeft

                        RightInsideTwoBar ->
                            Just SecondStickerFromRight

                        LeftOutsideTwoBar ->
                            Just FirstStickerFromLeft

                        RightOutsideTwoBar ->
                            Just FirstStickerFromRight
    in
    representativeSticker
        |> Maybe.map
            (\sticker ->
                case angle of
                    UFR ->
                        case sticker of
                            FirstStickerFromLeft ->
                                ( Cube.Advanced.FrontOrBack Cube.Advanced.F
                                , noPostAUFCaseRendering.ufl.f
                                )

                            SecondStickerFromLeft ->
                                ( Cube.Advanced.FrontOrBack Cube.Advanced.F
                                , noPostAUFCaseRendering.uf.f
                                )

                            ThirdStickerFromLeft ->
                                ( Cube.Advanced.FrontOrBack Cube.Advanced.F
                                , noPostAUFCaseRendering.ufr.f
                                )

                            ThirdStickerFromRight ->
                                ( Cube.Advanced.LeftOrRight Cube.Advanced.R
                                , noPostAUFCaseRendering.ufr.r
                                )

                            SecondStickerFromRight ->
                                ( Cube.Advanced.LeftOrRight Cube.Advanced.R
                                , noPostAUFCaseRendering.ur.r
                                )

                            FirstStickerFromRight ->
                                ( Cube.Advanced.LeftOrRight Cube.Advanced.R
                                , noPostAUFCaseRendering.ubr.r
                                )

                    UFL ->
                        case sticker of
                            FirstStickerFromLeft ->
                                ( Cube.Advanced.LeftOrRight Cube.Advanced.L
                                , noPostAUFCaseRendering.ubl.l
                                )

                            SecondStickerFromLeft ->
                                ( Cube.Advanced.LeftOrRight Cube.Advanced.L
                                , noPostAUFCaseRendering.ul.l
                                )

                            ThirdStickerFromLeft ->
                                ( Cube.Advanced.LeftOrRight Cube.Advanced.L
                                , noPostAUFCaseRendering.ufl.l
                                )

                            ThirdStickerFromRight ->
                                ( Cube.Advanced.FrontOrBack Cube.Advanced.F
                                , noPostAUFCaseRendering.ufl.f
                                )

                            SecondStickerFromRight ->
                                ( Cube.Advanced.FrontOrBack Cube.Advanced.F
                                , noPostAUFCaseRendering.uf.f
                                )

                            FirstStickerFromRight ->
                                ( Cube.Advanced.FrontOrBack Cube.Advanced.F
                                , noPostAUFCaseRendering.ufr.f
                                )
            )
        |> Maybe.andThen
            (\( originalFace, color ) ->
                Cube.Advanced.colorToFaceItBelongsTo color
                    |> Maybe.map
                        (\finalFace ->
                            { element = element
                            , originalFace = originalFace
                            , finalFace = finalFace
                            }
                        )
            )


getUfrReferenceAlgorithmCaseSpec : AUF -> PLL -> CaseRecognitionSpecification
getUfrReferenceAlgorithmCaseSpec ufrReferenceAlgorithmPreAUF pll =
    let
        nonempty =
            List.Nonempty.Nonempty

        singleton =
            List.Nonempty.singleton
    in
    case pll of
        H ->
            { emptyCaseSpec
                | patterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                , oppositelyColored =
                    [ ( singleton <| Pattern LeftHeadlights
                      , singleton <| Sticker SecondStickerFromLeft
                      )
                    , ( singleton <| Pattern RightHeadlights
                      , singleton <| Sticker SecondStickerFromRight
                      )
                    ]
            }

        Ua ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                        , notOppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                        , absentPatterns = Just <| singleton SixChecker
                        , noOtherBlocksPresent = True
                        , identicallyColored =
                            Just
                                ( Pattern RightHeadlights
                                , Sticker SecondStickerFromLeft
                                , []
                                )
                        , notOppositelyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftThreeBar [ RightHeadlights ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightThreeBar ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

        Ub ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                        , absentPatterns = Just <| singleton SixChecker
                        , noOtherBlocksPresent = True
                        , identicallyColored =
                            Just
                                ( Pattern LeftHeadlights
                                , Sticker SecondStickerFromRight
                                , []
                                )
                        , notOppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                        , notOppositelyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightHeadlights [ LeftThreeBar ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightThreeBar [ LeftHeadlights ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

        Z ->
            let
                none =
                    { emptyCaseSpec
                        | patterns = Just <| singleton SixChecker
                    }

                u =
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                        , noOtherStickersMatchThanThese =
                            Just <|
                                nonempty
                                    (Pattern LeftHeadlights)
                                    [ Pattern RightHeadlights ]
                        , notOppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }
            in
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    none

                AUF.Clockwise ->
                    u

                AUF.Halfway ->
                    none

                AUF.CounterClockwise ->
                    u

        Aa ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftOutsideTwoBar [ Bookends ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftOutsideTwoBar
                              , singleton <| Sticker ThirdStickerFromLeft
                              )
                            ]
                        , differentlyColored =
                            Just
                                ( Sticker ThirdStickerFromLeft
                                , Sticker SecondStickerFromRight
                                , []
                                )
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty Bookends [ LeftInsideTwoBar, RightInsideTwoBar ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightInsideTwoBar
                              , singleton <| Sticker FirstStickerFromRight
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftFourChecker [ RightOutsideTwoBar ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton RightHeadlights
                        , absentPatterns =
                            Just <|
                                nonempty
                                    RightFourChecker
                                    [ LeftHeadlights ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

        Ab ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightOutsideTwoBar [ Bookends ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightOutsideTwoBar
                              , singleton <| Sticker ThirdStickerFromRight
                              )
                            ]
                        , differentlyColored =
                            Just
                                ( Sticker ThirdStickerFromRight
                                , Sticker SecondStickerFromLeft
                                , []
                                )
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty Bookends [ LeftInsideTwoBar, RightInsideTwoBar ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftInsideTwoBar
                              , singleton <| Sticker FirstStickerFromLeft
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightFourChecker [ LeftOutsideTwoBar ]
                    }

                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton LeftHeadlights
                        , absentPatterns =
                            Just <|
                                nonempty
                                    LeftFourChecker
                                    [ RightHeadlights ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

        E ->
            let
                u =
                    { emptyCaseSpec
                        | absentPatterns =
                            Just <|
                                nonempty
                                    Bookends
                                    [ LeftHeadlights, RightHeadlights ]
                        , noOtherBlocksPresent = True
                        , identicallyColored =
                            Just
                                ( Sticker ThirdStickerFromLeft
                                , Sticker SecondStickerFromRight
                                , []
                                )
                        , differentlyColored =
                            Just
                                ( Sticker SecondStickerFromLeft
                                , Sticker ThirdStickerFromRight
                                , []
                                )
                    }
            in
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    mirrorCaseSpec u

                AUF.Clockwise ->
                    u

                AUF.Halfway ->
                    mirrorCaseSpec u

                AUF.CounterClockwise ->
                    u

        F ->
            let
                none =
                    { emptyCaseSpec
                        | patterns = Just <| singleton LeftThreeBar
                        , noOtherStickersMatchThanThese =
                            Just <|
                                singleton
                                    (Pattern LeftThreeBar)
                    }

                u2 =
                    { emptyCaseSpec
                        | patterns =
                            Just <|
                                nonempty
                                    Bookends
                                    [ InnerFourChecker ]
                        , oppositelyColored =
                            [ ( singleton <| Sticker FirstStickerFromRight
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }
            in
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    none

                AUF.Clockwise ->
                    mirrorCaseSpec none

                AUF.Halfway ->
                    u2

                AUF.CounterClockwise ->
                    mirrorCaseSpec u2

        T ->
            let
                u2 =
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightInsideTwoBar [ LeftHeadlights ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

                none =
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightOutsideTwoBar [ Bookends ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern RightOutsideTwoBar
                              , singleton <| Sticker ThirdStickerFromRight
                              )
                            ]
                        , differentlyColored =
                            Just
                                ( Sticker ThirdStickerFromRight
                                , Sticker SecondStickerFromLeft
                                , []
                                )
                    }
            in
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    none

                AUF.Clockwise ->
                    mirrorCaseSpec none

                AUF.Halfway ->
                    u2

                AUF.CounterClockwise ->
                    mirrorCaseSpec u2

        Ga ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty Bookends [ LeftInsideTwoBar ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern Bookends
                              , singleton <| Pattern LeftInsideTwoBar
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightOutsideTwoBar ]
                        , noOtherBlocksPresent = True
                        , differentlyColored =
                            Just
                                ( Sticker SecondStickerFromLeft
                                , Sticker ThirdStickerFromRight
                                , []
                                )
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton RightFourChecker
                        , absentPatterns = Just <| nonempty LeftHeadlights [ RightFiveChecker ]
                        , noOtherBlocksPresent = True
                    }

                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton Bookends
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern Bookends
                              , nonempty
                                    (Sticker ThirdStickerFromLeft)
                                    [ Sticker SecondStickerFromRight ]
                              )
                            ]
                        , differentlyColored =
                            Just
                                ( Sticker SecondStickerFromLeft
                                , Sticker ThirdStickerFromRight
                                , []
                                )
                    }

        _ ->
            emptyCaseSpec


mirrorCaseSpec : CaseRecognitionSpecification -> CaseRecognitionSpecification
mirrorCaseSpec spec =
    { patterns = Maybe.map (List.Nonempty.map mirrorPattern) spec.patterns
    , absentPatterns = Maybe.map (List.Nonempty.map mirrorPattern) spec.absentPatterns
    , oppositelyColored =
        List.map
            (Tuple.mapBoth
                (List.Nonempty.map mirrorElement)
                (List.Nonempty.map mirrorElement)
            )
            spec.oppositelyColored
    , notOppositelyColored =
        List.map
            (Tuple.mapBoth
                (List.Nonempty.map mirrorElement)
                (List.Nonempty.map mirrorElement)
            )
            spec.notOppositelyColored
    , adjacentlyColored =
        List.map
            (Tuple.mapBoth
                (List.Nonempty.map mirrorElement)
                (List.Nonempty.map mirrorElement)
            )
            spec.adjacentlyColored
    , identicallyColored = Maybe.map (mapMinLength2 mirrorElement) spec.identicallyColored
    , differentlyColored = Maybe.map (mapMinLength2 mirrorElement) spec.differentlyColored
    , noOtherStickersMatchThanThese =
        Maybe.map
            (List.Nonempty.map mirrorElement)
            spec.noOtherStickersMatchThanThese
    , noOtherBlocksPresent = spec.noOtherBlocksPresent
    }


mapMinLength2 : (a -> b) -> ( a, a, List a ) -> ( b, b, List b )
mapMinLength2 f ( first, second, rest ) =
    ( f first, f second, List.map f rest )


mirrorElement : RecognitionElement -> RecognitionElement
mirrorElement x =
    case x of
        Pattern pattern ->
            Pattern <| mirrorPattern pattern

        Sticker sticker ->
            Sticker
                (case sticker of
                    FirstStickerFromLeft ->
                        FirstStickerFromRight

                    FirstStickerFromRight ->
                        FirstStickerFromLeft

                    SecondStickerFromLeft ->
                        SecondStickerFromRight

                    SecondStickerFromRight ->
                        SecondStickerFromLeft

                    ThirdStickerFromLeft ->
                        ThirdStickerFromRight

                    ThirdStickerFromRight ->
                        ThirdStickerFromLeft
                )


mirrorPattern : RecognitionPattern -> RecognitionPattern
mirrorPattern pattern =
    case pattern of
        Bookends ->
            Bookends

        LeftHeadlights ->
            RightHeadlights

        RightHeadlights ->
            LeftHeadlights

        LeftThreeBar ->
            RightThreeBar

        RightThreeBar ->
            LeftThreeBar

        LeftInsideTwoBar ->
            RightInsideTwoBar

        RightInsideTwoBar ->
            LeftInsideTwoBar

        LeftOutsideTwoBar ->
            RightOutsideTwoBar

        RightOutsideTwoBar ->
            LeftOutsideTwoBar

        LeftFourChecker ->
            RightFourChecker

        RightFourChecker ->
            LeftFourChecker

        InnerFourChecker ->
            InnerFourChecker

        LeftFiveChecker ->
            RightFiveChecker

        RightFiveChecker ->
            LeftFiveChecker

        SixChecker ->
            SixChecker
