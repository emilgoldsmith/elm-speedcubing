module PLL exposing
    ( PLL(..), all
    , getLetters, solvedBy, getAllEquivalentAUFs, getAllAUFEquivalencyClasses
    , RecognitionSpecification, CaseRecognitionSpecification, PostAUFRecognitionSpecification, RecognitionElement(..), RecognitionPattern(..), Sticker(..)
    , getUniqueTwoSidedRecognitionSpecification, RecognitionAngle, uflRecognitionAngle, ufrRecognitionAngle, RecognitionError(..)
    , getSymmetry, PLLWithSymmetryInfo(..), FullySymmetricPLL(..), HalfSymmetricPLL(..), NPermSymmetricPLL(..), NonSymmetricPLL(..), pllWithSymmetryInfoToPLL, fullySymmetricPLLToPLL, halfSymmetricPLLToPLL, nPermSymmetricPLLToPLL, nonSymmetricPLLToPLL
    , Algorithms, getAlgorithm, referenceAlgorithms
    )

{-| Types and helper functions to work with the Permutate Last
Layer (PLL) algorithm set, the last step of the CFOP method. See
<https://www.speedsolving.com/wiki/index.php/PLL>
for further information


# Definition And Constructors

@docs PLL, all


# Helpers

@docs getLetters, solvedBy, getAllEquivalentAUFs, getAllAUFEquivalencyClasses


# Two Sided Recognition

@docs RecognitionSpecification, CaseRecognitionSpecification, PostAUFRecognitionSpecification, RecognitionElement, RecognitionPattern, Sticker
@docs getUniqueTwoSidedRecognitionSpecification, RecognitionAngle, uflRecognitionAngle, ufrRecognitionAngle, RecognitionError


# Symmetry

@docs getSymmetry, PLLWithSymmetryInfo, FullySymmetricPLL, HalfSymmetricPLL, NPermSymmetricPLL, NonSymmetricPLL, pllWithSymmetryInfoToPLL, fullySymmetricPLLToPLL, halfSymmetricPLLToPLL, nPermSymmetricPLLToPLL, nonSymmetricPLLToPLL


# Collections

@docs Algorithms, getAlgorithm, referenceAlgorithms

-}

import AUF exposing (AUF)
import Algorithm exposing (Algorithm)
import Cube
import Cube.Advanced
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
                NotSymmetric _ ->
                    AUF.all

                HalfSymmetric _ ->
                    List.Nonempty.Nonempty AUF.None [ AUF.Clockwise ]

                NPermSymmetric _ ->
                    List.Nonempty.singleton AUF.None

                FullySymmetric _ ->
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

    getAllEquivalentAUFs ( AUF.None, H, AUF.None )
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
        NotSymmetric _ ->
            List.Nonempty.singleton ( preAUF, postAUF )

        HalfSymmetric _ ->
            List.Nonempty.Nonempty
                ( preAUF, postAUF )
                [ ( AUF.add preAUF AUF.Halfway, AUF.add postAUF AUF.Halfway ) ]

        NPermSymmetric _ ->
            AUF.all
                |> List.Nonempty.map
                    (\toAdd ->
                        ( AUF.add preAUF toAdd, AUF.add postAUF toAdd )
                    )

        FullySymmetric _ ->
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
AUF pairs for the given The equivalency classes are also exhaustive, so every
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


{-| A PLL with the classification of it by the symmetry patterns of the case.
The descriptions of the different classes of symmetries can be found below
-}
type PLLWithSymmetryInfo
    = FullySymmetric FullySymmetricPLL
    | HalfSymmetric HalfSymmetricPLL
    | NPermSymmetric NPermSymmetricPLL
    | NotSymmetric NonSymmetricPLL


{-| A fully symmetric PLL where you can use the same algorithm to solve
it from any angle and doing a preAUF and a postAUF are equivalent.
For example (U, pll-alg, U) is equivalent to (no-auf, pll-alg, U2)
-}
type FullySymmetricPLL
    = FullSymH


{-| Convert a fully symmetrical PLL with symmetry info to a normal PLL type
-}
fullySymmetricPLLToPLL : FullySymmetricPLL -> PLL
fullySymmetricPLLToPLL fullSym =
    case fullSym of
        FullSymH ->
            H


{-| A half symmetric PLL which has the same AUF properties as a fully symmetric PLL
but only opposing faces can be solved with the same PLL algorithm, so AUF transformations
can also only happen by adding or subtracting U2s to the AUFs
-}
type HalfSymmetricPLL
    = HalfSymZ
    | HalfSymE


{-| Convert a half symmetrical PLL with symmetry info to a normal PLL type
-}
halfSymmetricPLLToPLL : HalfSymmetricPLL -> PLL
halfSymmetricPLLToPLL halfSym =
    case halfSym of
        HalfSymZ ->
            Z

        HalfSymE ->
            E


{-| An N-Perm symmetric PLL (which only includes the N-Perms) can like fully symmetric PLLs
be solved with the same algorithm from any angle, but here a transformation through pre-AUF
causes the inverse transformation for the post-AUF. It is best explained with an example: With
the same example of (U, pll-alg, U) from the fully symmetric case, with an N-perm it would now
be equivalent to (no-auf, pll-alg, no-auf)
-}
type NPermSymmetricPLL
    = NPermSymNa
    | NPermSymNb


{-| Convert an N-perm symmetrical PLL with symmetry info to a normal PLL type
-}
nPermSymmetricPLLToPLL : NPermSymmetricPLL -> PLL
nPermSymmetricPLLToPLL nPermSym =
    case nPermSym of
        NPermSymNa ->
            Na

        NPermSymNb ->
            Nb


{-| A non-symmetric PLL. Any given PLL algorithm can only solve this case from a single angle
and therefore no AUF transformations make sense in this case either
-}
type NonSymmetricPLL
    = NotSymUa
    | NotSymUb
    | NotSymAa
    | NotSymAb
    | NotSymF
    | NotSymGa
    | NotSymGb
    | NotSymGc
    | NotSymGd
    | NotSymJa
    | NotSymJb
    | NotSymRa
    | NotSymRb
    | NotSymT
    | NotSymV
    | NotSymY


{-| Convert a non-symmetrical PLL with symmetry info to a normal PLL type
-}
nonSymmetricPLLToPLL : NonSymmetricPLL -> PLL
nonSymmetricPLLToPLL nonSym =
    case nonSym of
        NotSymUa ->
            Ua

        NotSymUb ->
            Ub

        NotSymAa ->
            Aa

        NotSymAb ->
            Ab

        NotSymF ->
            F

        NotSymGa ->
            Ga

        NotSymGb ->
            Gb

        NotSymGc ->
            Gc

        NotSymGd ->
            Gd

        NotSymJa ->
            Ja

        NotSymJb ->
            Jb

        NotSymRa ->
            Ra

        NotSymRb ->
            Rb

        NotSymT ->
            T

        NotSymV ->
            V

        NotSymY ->
            Y


{-| Convert a PLL with symmetry info to a normal PLL type
-}
pllWithSymmetryInfoToPLL : PLLWithSymmetryInfo -> PLL
pllWithSymmetryInfoToPLL pllWithSym =
    case pllWithSym of
        FullySymmetric fullSym ->
            fullySymmetricPLLToPLL fullSym

        HalfSymmetric halfSym ->
            halfSymmetricPLLToPLL halfSym

        NPermSymmetric nPermSym ->
            nPermSymmetricPLLToPLL nPermSym

        NotSymmetric nonSym ->
            nonSymmetricPLLToPLL nonSym


{-| Get the type of symmetry this PLL case displays
-}
getSymmetry : PLL -> PLLWithSymmetryInfo
getSymmetry pll =
    case pll of
        H ->
            FullySymmetric FullSymH

        Ua ->
            NotSymmetric NotSymUa

        Ub ->
            NotSymmetric NotSymUb

        Z ->
            HalfSymmetric HalfSymZ

        Aa ->
            NotSymmetric NotSymAa

        Ab ->
            NotSymmetric NotSymAb

        E ->
            HalfSymmetric HalfSymE

        F ->
            NotSymmetric NotSymF

        Ga ->
            NotSymmetric NotSymGa

        Gb ->
            NotSymmetric NotSymGb

        Gc ->
            NotSymmetric NotSymGc

        Gd ->
            NotSymmetric NotSymGd

        Ja ->
            NotSymmetric NotSymJa

        Jb ->
            NotSymmetric NotSymJb

        Na ->
            NPermSymmetric NPermSymNa

        Nb ->
            NPermSymmetric NPermSymNb

        Ra ->
            NotSymmetric NotSymRa

        Rb ->
            NotSymmetric NotSymRb

        T ->
            NotSymmetric NotSymT

        V ->
            NotSymmetric NotSymV

        Y ->
            NotSymmetric NotSymY



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


{-| Describes a unique way to recognize the pll case from just the two sides
given by the RecognitionAngle.
It also describes one or more possibilities for how to recognize which way to
do the final AUF.
-}
type alias RecognitionSpecification =
    { caseRecognition : CaseRecognitionSpecification
    , postAUFRecognition : PostAUFRecognitionSpecification
    }


{-| Describes a unique way to recognize the pll case from just the two sides given by the RecognitionAngle
-}
type alias CaseRecognitionSpecification =
    { patterns : Maybe (List.Nonempty.Nonempty RecognitionPattern)
    , absentPatterns : Maybe (List.Nonempty.Nonempty RecognitionPattern)
    , oppositelyColored : List ( List.Nonempty.Nonempty RecognitionElement, List.Nonempty.Nonempty RecognitionElement )
    , adjacentlyColored : List ( List.Nonempty.Nonempty RecognitionElement, List.Nonempty.Nonempty RecognitionElement )
    , identicallyColored : List ( RecognitionElement, RecognitionElement, List RecognitionElement )
    , differentlyColored : List ( RecognitionElement, RecognitionElement, List RecognitionElement )
    , noOtherStickersMatchThanThese : Maybe (List.Nonempty.Nonempty RecognitionElement)
    , noOtherBlocksPresent : Bool
    }


emptyCaseSpec : CaseRecognitionSpecification
emptyCaseSpec =
    { patterns = Nothing
    , absentPatterns = Nothing
    , oppositelyColored = []
    , adjacentlyColored = []
    , identicallyColored = []
    , differentlyColored = []
    , noOtherStickersMatchThanThese = Nothing
    , noOtherBlocksPresent = False
    }


{-| Describes one or more possibilities for how to recognize which way to do the final AUF
-}
type alias PostAUFRecognitionSpecification =
    List.Nonempty.Nonempty
        { elementsWithOriginalFace :
            List.Nonempty.Nonempty
                ( RecognitionElement
                , Cube.Advanced.Face
                )
        , finalFace : Cube.Advanced.Face
        }


{-| Either a type of pattern on the two sides or just a single sticker on
the two sides.
-}
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


{-| All the possible patterns that we use to recognize the pll case.
-}
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


allPatterns : List.Nonempty.Nonempty RecognitionPattern
allPatterns =
    let
        fromLeftHeadlights pattern =
            case pattern of
                LeftHeadlights ->
                    Just RightHeadlights

                RightHeadlights ->
                    Just LeftThreeBar

                LeftThreeBar ->
                    Just RightThreeBar

                RightThreeBar ->
                    Just LeftInsideTwoBar

                LeftInsideTwoBar ->
                    Just RightInsideTwoBar

                RightInsideTwoBar ->
                    Just LeftOutsideTwoBar

                LeftOutsideTwoBar ->
                    Just RightOutsideTwoBar

                RightOutsideTwoBar ->
                    Just Bookends

                Bookends ->
                    Just LeftFourChecker

                LeftFourChecker ->
                    Just RightFourChecker

                RightFourChecker ->
                    Just InnerFourChecker

                InnerFourChecker ->
                    Just LeftFiveChecker

                LeftFiveChecker ->
                    Just RightFiveChecker

                RightFiveChecker ->
                    Just SixChecker

                SixChecker ->
                    Nothing
    in
    Utils.Enumerator.from LeftHeadlights fromLeftHeadlights


{-| The six visible stickers during two sided recognition.
-}
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


type alias RecognitionStickerColors =
    { firstFromLeft : Cube.Advanced.CubeColor
    , secondFromLeft : Cube.Advanced.CubeColor
    , thirdFromLeft : Cube.Advanced.CubeColor
    , firstFromRight : Cube.Advanced.CubeColor
    , secondFromRight : Cube.Advanced.CubeColor
    , thirdFromRight : Cube.Advanced.CubeColor
    }


getRecognitionStickers :
    { pllAlgorithmUsed : Algorithm, recognitionAngle : RecognitionAngle, preAUF : AUF, pll : PLL }
    -> RecognitionStickerColors
getRecognitionStickers params =
    let
        rotationToGetCorrectRecognitionAngle =
            if params.recognitionAngle == ufrRecognitionAngle then
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


isPatternPresent : RecognitionStickerColors -> RecognitionPattern -> Bool
isPatternPresent colors pattern =
    case pattern of
        Bookends ->
            colors.firstFromLeft == colors.firstFromRight

        LeftHeadlights ->
            colors.firstFromLeft == colors.thirdFromLeft

        RightHeadlights ->
            colors.firstFromRight == colors.thirdFromRight

        LeftThreeBar ->
            (colors.firstFromLeft == colors.secondFromLeft)
                && (colors.secondFromLeft == colors.thirdFromLeft)

        RightThreeBar ->
            (colors.firstFromRight == colors.secondFromRight)
                && (colors.secondFromRight == colors.thirdFromRight)

        LeftInsideTwoBar ->
            colors.secondFromLeft == colors.thirdFromLeft

        RightInsideTwoBar ->
            colors.secondFromRight == colors.thirdFromRight

        LeftOutsideTwoBar ->
            colors.firstFromLeft == colors.secondFromLeft

        RightOutsideTwoBar ->
            colors.firstFromRight == colors.secondFromRight

        LeftFourChecker ->
            (colors.firstFromLeft == colors.thirdFromLeft)
                && (colors.secondFromLeft == colors.thirdFromRight)

        RightFourChecker ->
            (colors.firstFromRight == colors.thirdFromRight)
                && (colors.secondFromRight == colors.thirdFromLeft)

        InnerFourChecker ->
            (colors.secondFromLeft == colors.thirdFromRight)
                && (colors.thirdFromLeft == colors.secondFromRight)

        LeftFiveChecker ->
            (colors.firstFromLeft == colors.thirdFromLeft)
                && (colors.secondFromLeft == colors.thirdFromRight)
                && (colors.thirdFromLeft == colors.secondFromRight)

        RightFiveChecker ->
            (colors.firstFromRight == colors.thirdFromRight)
                && (colors.secondFromRight == colors.thirdFromLeft)
                && (colors.thirdFromRight == colors.secondFromLeft)

        SixChecker ->
            (colors.firstFromLeft == colors.thirdFromLeft)
                && (colors.secondFromLeft == colors.thirdFromRight)
                && (colors.thirdFromLeft == colors.secondFromRight)
                && (colors.thirdFromRight == colors.firstFromRight)


{-| The angle the cube is being looked at while doing two sided recognition
-}
type RecognitionAngle
    = UFR
    | UFL


{-| Describes looking at the cube so that the U F and R sides are visible
-}
ufrRecognitionAngle : RecognitionAngle
ufrRecognitionAngle =
    UFR


{-| Describes looking at the cube so that the U F and L sides are visible
-}
uflRecognitionAngle : RecognitionAngle
uflRecognitionAngle =
    UFL


{-| Occurs if the pll algorithm provided to
getUniqueTwoSidedRecognitionSpecification for the case does not solve the case
-}
type RecognitionError
    = IncorrectPLLAlgorithm PLL Algorithm


{-| Gets a two sided recognition specification that uniquely identifies the pll case and preAUF
-}
getUniqueTwoSidedRecognitionSpecification :
    { pllAlgorithmUsed : Algorithm, recognitionAngle : RecognitionAngle, preAUF : AUF, pll : PLL }
    -> Result RecognitionError RecognitionSpecification
getUniqueTwoSidedRecognitionSpecification params =
    let
        toAddForReferenceAlgorithm =
            Cube.detectAUFs
                { toDetectFor = getAlgorithm referenceAlgorithms params.pll
                , toMatchTo = params.pllAlgorithmUsed
                }
                |> Maybe.map Tuple.first
                |> Result.fromMaybe
                    (IncorrectPLLAlgorithm params.pll params.pllAlgorithmUsed)

        toAddForUFRAngle =
            case params.recognitionAngle of
                UFR ->
                    AUF.None

                UFL ->
                    AUF.Clockwise

        ufrReferenceAlgorithmPreAUFResult =
            toAddForReferenceAlgorithm
                |> Result.map (AUF.add toAddForUFRAngle)
                |> Result.map (AUF.add params.preAUF)

        caseSpecResult =
            ufrReferenceAlgorithmPreAUFResult
                |> Result.map
                    (\ufrReferenceAlgorithmPreAUF ->
                        getUfrReferenceAlgorithmCaseSpec ufrReferenceAlgorithmPreAUF params.pll
                    )
    in
    caseSpecResult
        |> Result.map
            (\caseSpec ->
                { caseRecognition = caseSpec
                , postAUFRecognition = getPostAUFRecognition params
                }
            )


getPostAUFRecognition :
    { pllAlgorithmUsed : Algorithm, recognitionAngle : RecognitionAngle, preAUF : AUF, pll : PLL }
    -> PostAUFRecognitionSpecification
getPostAUFRecognition params =
    let
        recognitionStickers =
            getRecognitionStickers params

        allRelevantElements =
            allPatterns
                |> List.Nonempty.toList
                |> List.filter (isPatternPresent recognitionStickers)
                |> List.map Pattern
                |> List.Nonempty.Extra.appendListBehind
                    (List.Nonempty.map Sticker allStickers)

        elementsWithTargets =
            allRelevantElements
                |> List.Nonempty.toList
                |> List.filterMap
                    (getOriginalAndTargetFace
                        { noPostAUFRecognitionStickers = recognitionStickers }
                        params.recognitionAngle
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
                                    Just
                                        { pattern = pattern
                                        , orignalFace = originalFace
                                        , finalFace = finalFace
                                        }

                                else
                                    Nothing
                    )
    in
    -- If a pattern stays in place we want the most visible of those
    -- as this is obviously the easiest to recognize post AUF with
    case
        List.head <|
            List.sortBy (.pattern >> howVisibleIsPattern) <|
                patternsThatStayInPlace
    of
        Just patternThatStaysInPlace ->
            List.Nonempty.singleton <|
                { elementsWithOriginalFace =
                    List.Nonempty.singleton
                        ( Pattern patternThatStaysInPlace.pattern
                        , patternThatStaysInPlace.orignalFace
                        )
                , finalFace = patternThatStaysInPlace.finalFace
                }

        Nothing ->
            let
                groupedElements =
                    elementsWithTargets
                        |> List.foldl
                            (\({ originalFace, finalFace } as cur) acc ->
                                if originalFace == finalFace then
                                    { acc
                                        | staysInPlace = cur :: acc.staysInPlace
                                    }

                                else if faceVisible params.recognitionAngle finalFace then
                                    { acc
                                        | switchesToOtherVisibleSide =
                                            cur :: acc.switchesToOtherVisibleSide
                                    }

                                else
                                    { acc
                                        | rest = cur :: acc.rest
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
                        List.sortBy
                            (.elementsWithOriginalFace
                                >> List.Nonempty.map Tuple.first
                                >> howVisibleIsElementGroup
                            )
                            stickersAlsoGrouped.staysInPlace
                    , switchesToOtherVisibleSide =
                        List.sortBy
                            (.elementsWithOriginalFace
                                >> List.Nonempty.map Tuple.first
                                >> howVisibleIsElementGroup
                            )
                            stickersAlsoGrouped.switchesToOtherVisibleSide
                    , rest =
                        List.sortBy
                            (.elementsWithOriginalFace
                                >> List.Nonempty.map Tuple.first
                                >> howVisibleIsElementGroup
                            )
                            stickersAlsoGrouped.rest
                    }

                finalList =
                    [ List.head sortedGroupedElements.staysInPlace
                    , List.head sortedGroupedElements.switchesToOtherVisibleSide
                    , List.head sortedGroupedElements.rest
                    ]
                        |> List.filterMap identity
                        -- The order here of the input list and of the fold direction are important.
                        -- We want to be iterating from the best target face (stays in place)
                        -- to the worst (rest)
                        |> List.foldl
                            (\next acc ->
                                let
                                    nextHasPattern =
                                        next.elementsWithOriginalFace
                                            |> List.Nonempty.any (Tuple.first >> isPattern)

                                    isMoreVisibleThanPreviousGroups =
                                        acc
                                            |> List.all
                                                (\previousGroup ->
                                                    nextHasPattern
                                                        && not
                                                            (previousGroup.elementsWithOriginalFace
                                                                |> List.Nonempty.any (Tuple.first >> isPattern)
                                                            )
                                                )
                                in
                                if isMoreVisibleThanPreviousGroups then
                                    next :: acc

                                else
                                    acc
                            )
                            []
                        -- Keep the order as the :: operator reverses it
                        |> List.reverse
            in
            finalList
                |> List.Nonempty.fromList
                |> Maybe.withDefault
                    -- This should never happen so make a nonsense case that should
                    -- definitely fail tests and obviously be wrong
                    (List.Nonempty.Nonempty
                        { elementsWithOriginalFace =
                            List.Nonempty.singleton
                                ( Pattern LeftThreeBar, Cube.Advanced.UpOrDown Cube.Advanced.U )
                        , finalFace = Cube.Advanced.FrontOrBack Cube.Advanced.F
                        }
                        [ { elementsWithOriginalFace =
                                List.Nonempty.singleton
                                    ( Pattern SixChecker, Cube.Advanced.UpOrDown Cube.Advanced.U )
                          , finalFace = Cube.Advanced.FrontOrBack Cube.Advanced.F
                          }
                        ]
                    )


getOriginalAndTargetFace :
    { noPostAUFRecognitionStickers : RecognitionStickerColors }
    -> RecognitionAngle
    -> RecognitionElement
    -> Maybe { element : RecognitionElement, originalFace : Cube.Advanced.Face, finalFace : Cube.Advanced.Face }
getOriginalAndTargetFace { noPostAUFRecognitionStickers } angle element =
    let
        maybePositionAndColor =
            case element of
                Sticker sticker ->
                    case sticker of
                        FirstStickerFromLeft ->
                            Just { onLeftSide = True, color = noPostAUFRecognitionStickers.firstFromLeft }

                        SecondStickerFromLeft ->
                            Just { onLeftSide = True, color = noPostAUFRecognitionStickers.secondFromLeft }

                        ThirdStickerFromLeft ->
                            Just { onLeftSide = True, color = noPostAUFRecognitionStickers.thirdFromLeft }

                        FirstStickerFromRight ->
                            Just { onLeftSide = False, color = noPostAUFRecognitionStickers.firstFromRight }

                        SecondStickerFromRight ->
                            Just { onLeftSide = False, color = noPostAUFRecognitionStickers.secondFromRight }

                        ThirdStickerFromRight ->
                            Just { onLeftSide = False, color = noPostAUFRecognitionStickers.thirdFromRight }

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
                            Just { onLeftSide = True, color = noPostAUFRecognitionStickers.firstFromLeft }

                        RightHeadlights ->
                            Just { onLeftSide = False, color = noPostAUFRecognitionStickers.firstFromRight }

                        LeftThreeBar ->
                            Just { onLeftSide = True, color = noPostAUFRecognitionStickers.firstFromLeft }

                        RightThreeBar ->
                            Just { onLeftSide = False, color = noPostAUFRecognitionStickers.firstFromRight }

                        LeftInsideTwoBar ->
                            Just { onLeftSide = True, color = noPostAUFRecognitionStickers.secondFromLeft }

                        RightInsideTwoBar ->
                            Just { onLeftSide = False, color = noPostAUFRecognitionStickers.secondFromRight }

                        LeftOutsideTwoBar ->
                            Just { onLeftSide = True, color = noPostAUFRecognitionStickers.firstFromLeft }

                        RightOutsideTwoBar ->
                            Just { onLeftSide = False, color = noPostAUFRecognitionStickers.firstFromRight }
    in
    maybePositionAndColor
        |> Maybe.map
            (\{ onLeftSide, color } ->
                case angle of
                    UFR ->
                        if onLeftSide then
                            ( Cube.Advanced.FrontOrBack Cube.Advanced.F, color )

                        else
                            ( Cube.Advanced.LeftOrRight Cube.Advanced.R, color )

                    UFL ->
                        if onLeftSide then
                            ( Cube.Advanced.LeftOrRight Cube.Advanced.L, color )

                        else
                            ( Cube.Advanced.FrontOrBack Cube.Advanced.F, color )
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


groupStickersByFinalFace :
    List
        { element : RecognitionElement
        , originalFace : Cube.Advanced.Face
        , finalFace : Cube.Advanced.Face
        }
    ->
        List
            { elementsWithOriginalFace :
                List.Nonempty.Nonempty
                    ( RecognitionElement
                    , Cube.Advanced.Face
                    )
            , finalFace : Cube.Advanced.Face
            }
groupStickersByFinalFace list =
    let
        groupedByFace =
            list
                |> List.foldl
                    (\{ element, originalFace, finalFace } acc ->
                        -- The order doesn't matter just that they get separated
                        case element of
                            Pattern _ ->
                                { acc
                                    | patterns =
                                        { elementsWithOriginalFace =
                                            List.Nonempty.singleton
                                                ( element
                                                , originalFace
                                                )
                                        , finalFace = finalFace
                                        }
                                            :: acc.patterns
                                }

                            Sticker sticker ->
                                case finalFace of
                                    Cube.Advanced.FrontOrBack Cube.Advanced.F ->
                                        { acc
                                            | f = ( sticker, originalFace ) :: acc.f
                                        }

                                    Cube.Advanced.FrontOrBack Cube.Advanced.B ->
                                        { acc
                                            | b = ( sticker, originalFace ) :: acc.b
                                        }

                                    Cube.Advanced.LeftOrRight Cube.Advanced.L ->
                                        { acc
                                            | l = ( sticker, originalFace ) :: acc.l
                                        }

                                    Cube.Advanced.LeftOrRight Cube.Advanced.R ->
                                        { acc
                                            | r = ( sticker, originalFace ) :: acc.r
                                        }

                                    Cube.Advanced.UpOrDown Cube.Advanced.U ->
                                        { acc
                                            | u = ( sticker, originalFace ) :: acc.u
                                        }

                                    Cube.Advanced.UpOrDown Cube.Advanced.D ->
                                        { acc
                                            | d = ( sticker, originalFace ) :: acc.d
                                        }
                    )
                    { u = []
                    , d = []
                    , l = []
                    , r = []
                    , f = []
                    , b = []
                    , patterns = []
                    }
    in
    groupedByFace.patterns
        ++ ([ ( groupedByFace.u, Cube.Advanced.UpOrDown Cube.Advanced.U )
            , ( groupedByFace.d, Cube.Advanced.UpOrDown Cube.Advanced.D )
            , ( groupedByFace.l, Cube.Advanced.LeftOrRight Cube.Advanced.L )
            , ( groupedByFace.r, Cube.Advanced.LeftOrRight Cube.Advanced.R )
            , ( groupedByFace.f, Cube.Advanced.FrontOrBack Cube.Advanced.F )
            , ( groupedByFace.b, Cube.Advanced.FrontOrBack Cube.Advanced.B )
            ]
                |> List.foldl
                    (\( elementsWithOriginalFace, finalFace ) acc ->
                        case List.Nonempty.fromList elementsWithOriginalFace of
                            Nothing ->
                                acc

                            Just nonemptyElementsWithOriginalFace ->
                                { elementsWithOriginalFace =
                                    nonemptyElementsWithOriginalFace
                                        |> List.Nonempty.map (Tuple.mapFirst Sticker)
                                , finalFace = finalFace
                                }
                                    :: acc
                    )
                    []
           )


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
                        -- all be worth the same and be sorted after patterns,
                        -- also sorted after patterns even
                        -- when several of them later are grouped together
                        -- which makes them more visible
                        1000

                    Pattern pattern ->
                        howVisibleIsPattern pattern
            )
        |> List.Nonempty.Extra.minimum
        -- Value groups more if there are for example more stickers with
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
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                        , absentPatterns = Just <| singleton SixChecker
                        , identicallyColored =
                            [ ( Pattern RightHeadlights
                              , Sticker SecondStickerFromLeft
                              , []
                              )
                            ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftThreeBar [ RightHeadlights ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightThreeBar ]
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
                        , identicallyColored =
                            [ ( Pattern LeftHeadlights
                              , Sticker SecondStickerFromRight
                              , []
                              )
                            ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightHeadlights [ LeftThreeBar ]
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightThreeBar [ LeftHeadlights ]
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
                        , adjacentlyColored =
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
                        | patterns = Just <| singleton LeftOutsideTwoBar
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftOutsideTwoBar
                              , singleton <| Sticker ThirdStickerFromLeft
                              )
                            ]
                        , differentlyColored =
                            [ ( Sticker ThirdStickerFromLeft
                              , Sticker SecondStickerFromRight
                              , []
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty Bookends [ LeftInsideTwoBar, RightInsideTwoBar ]
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
                        | patterns = Just <| singleton RightOutsideTwoBar
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightOutsideTwoBar
                              , singleton <| Sticker ThirdStickerFromRight
                              )
                            ]
                        , differentlyColored =
                            [ ( Sticker ThirdStickerFromRight
                              , Sticker SecondStickerFromLeft
                              , []
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty Bookends [ LeftInsideTwoBar, RightInsideTwoBar ]
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
                            [ ( Sticker ThirdStickerFromLeft
                              , Sticker SecondStickerFromRight
                              , []
                              )
                            ]
                        , differentlyColored =
                            [ ( Sticker SecondStickerFromLeft
                              , Sticker ThirdStickerFromRight
                              , []
                              )
                            ]
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
                            [ ( Sticker SecondStickerFromLeft
                              , Sticker ThirdStickerFromRight
                              , []
                              )
                            ]
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
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                        , differentlyColored =
                            [ ( Sticker SecondStickerFromLeft
                              , Sticker ThirdStickerFromRight
                              , []
                              )
                            ]
                    }

        Gb ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightOutsideTwoBar [ Bookends ]
                        , identicallyColored =
                            [ ( Sticker SecondStickerFromLeft
                              , Sticker ThirdStickerFromRight
                              , []
                              )
                            ]
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightOutsideTwoBar
                              , singleton <| Sticker ThirdStickerFromRight
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton LeftHeadlights
                        , absentPatterns = Just <| singleton RightHeadlights
                        , noOtherBlocksPresent = True
                        , differentlyColored =
                            [ ( Pattern LeftHeadlights
                              , Sticker SecondStickerFromRight
                              , []
                              )
                            ]
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton RightHeadlights
                        , absentPatterns = Just <| singleton LeftHeadlights
                        , identicallyColored =
                            [ ( Pattern RightHeadlights
                              , Sticker SecondStickerFromLeft
                              , []
                              )
                            ]
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftInsideTwoBar [ Bookends ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern Bookends
                              , singleton <| Pattern LeftInsideTwoBar
                              )
                            ]
                    }

        Gc ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton Bookends
                        , absentPatterns = Just <| singleton InnerFourChecker
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Sticker FirstStickerFromLeft
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton LeftFourChecker
                        , absentPatterns = Just <| nonempty RightHeadlights [ LeftFiveChecker ]
                        , noOtherBlocksPresent = True
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftOutsideTwoBar [ RightHeadlights ]
                        , noOtherBlocksPresent = True
                        , differentlyColored =
                            [ ( Sticker SecondStickerFromRight
                              , Sticker ThirdStickerFromLeft
                              , []
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightInsideTwoBar [ Bookends ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern Bookends
                              , singleton <| Pattern RightInsideTwoBar
                              )
                            ]
                    }

        Gd ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty RightInsideTwoBar [ Bookends ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern Bookends
                              , singleton <| Pattern RightInsideTwoBar
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton LeftHeadlights
                        , absentPatterns = Just <| singleton RightHeadlights
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                        , identicallyColored =
                            [ ( Pattern LeftHeadlights
                              , Sticker SecondStickerFromRight
                              , []
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton RightHeadlights
                        , absentPatterns = Just <| singleton LeftHeadlights
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                        , differentlyColored =
                            [ ( Pattern RightHeadlights
                              , Sticker SecondStickerFromLeft
                              , []
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftOutsideTwoBar [ Bookends ]
                        , noOtherBlocksPresent = True
                        , oppositelyColored =
                            [ ( singleton <| Pattern Bookends
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

        Ja ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftOutsideTwoBar [ RightThreeBar ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftOutsideTwoBar [ RightInsideTwoBar, Bookends ]
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftOutsideTwoBar
                              , singleton <| Sticker ThirdStickerFromLeft
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftOutsideTwoBar [ RightInsideTwoBar, Bookends ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern LeftOutsideTwoBar
                              , singleton <| Sticker ThirdStickerFromLeft
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftThreeBar [ RightInsideTwoBar ]
                    }

        Jb ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftInsideTwoBar [ RightOutsideTwoBar, Bookends ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern LeftInsideTwoBar
                              , singleton <| Sticker FirstStickerFromLeft
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftThreeBar [ RightOutsideTwoBar ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftInsideTwoBar [ RightThreeBar ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftInsideTwoBar [ RightOutsideTwoBar, Bookends ]
                        , oppositelyColored =
                            [ ( singleton <| Pattern LeftInsideTwoBar
                              , singleton <| Sticker FirstStickerFromLeft
                              )
                            ]
                    }

        Na ->
            { emptyCaseSpec
                | patterns = Just <| nonempty LeftInsideTwoBar [ RightOutsideTwoBar ]
                , absentPatterns = Just <| singleton Bookends
                , noOtherBlocksPresent = True
            }

        Nb ->
            { emptyCaseSpec
                | patterns = Just <| nonempty LeftOutsideTwoBar [ RightInsideTwoBar ]
                , absentPatterns = Just <| singleton Bookends
                , noOtherBlocksPresent = True
            }

        Ra ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton Bookends
                        , absentPatterns = Just <| singleton InnerFourChecker
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern Bookends
                              , nonempty
                                    (Sticker SecondStickerFromLeft)
                                    [ Sticker ThirdStickerFromRight ]
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftOutsideTwoBar [ Bookends ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern LeftOutsideTwoBar
                              , nonempty
                                    (Sticker ThirdStickerFromLeft)
                                    [ Sticker SecondStickerFromRight ]
                              )
                            ]
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftHeadlights [ RightInsideTwoBar ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern LeftHeadlights
                              , singleton <| Sticker SecondStickerFromLeft
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton RightFiveChecker
                        , absentPatterns = Just <| singleton SixChecker
                    }

        Rb ->
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton Bookends
                        , absentPatterns = Just <| singleton InnerFourChecker
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern Bookends
                              , nonempty
                                    (Sticker SecondStickerFromRight)
                                    [ Sticker ThirdStickerFromLeft ]
                              )
                            ]
                    }

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton LeftFiveChecker
                        , absentPatterns = Just <| singleton SixChecker
                    }

                AUF.Halfway ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftInsideTwoBar [ RightHeadlights ]
                        , noOtherBlocksPresent = True
                        , adjacentlyColored =
                            [ ( singleton <| Pattern RightHeadlights
                              , singleton <| Sticker SecondStickerFromRight
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty Bookends [ RightOutsideTwoBar ]
                        , adjacentlyColored =
                            [ ( singleton <| Pattern RightOutsideTwoBar
                              , nonempty
                                    (Sticker SecondStickerFromLeft)
                                    [ Sticker ThirdStickerFromRight ]
                              )
                            ]
                    }

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
                            [ ( Sticker ThirdStickerFromRight
                              , Sticker SecondStickerFromLeft
                              , []
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

        V ->
            let
                none =
                    { emptyCaseSpec
                        | patterns = Just <| singleton LeftOutsideTwoBar
                        , absentPatterns = Just <| nonempty Bookends [ RightHeadlights ]
                        , noOtherBlocksPresent = True
                    }
            in
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    none

                AUF.Clockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftInsideTwoBar [ RightInsideTwoBar ]
                        , absentPatterns = Just <| singleton Bookends
                        , noOtherBlocksPresent = True
                    }

                AUF.Halfway ->
                    mirrorCaseSpec none

                AUF.CounterClockwise ->
                    { emptyCaseSpec
                        | patterns = Just <| singleton InnerFourChecker
                        , absentPatterns = Just <| nonempty Bookends [ RightHeadlights, LeftHeadlights ]
                    }

        Y ->
            let
                u =
                    { emptyCaseSpec
                        | patterns = Just <| singleton RightInsideTwoBar
                        , absentPatterns = Just <| nonempty Bookends [ LeftHeadlights ]
                        , noOtherBlocksPresent = True
                    }
            in
            case ufrReferenceAlgorithmPreAUF of
                AUF.None ->
                    { emptyCaseSpec
                        | patterns = Just <| nonempty LeftOutsideTwoBar [ RightOutsideTwoBar ]
                        , noOtherBlocksPresent = True
                    }

                AUF.Clockwise ->
                    u

                AUF.Halfway ->
                    { emptyCaseSpec
                        | absentPatterns = Just <| nonempty LeftHeadlights [ RightHeadlights ]
                        , noOtherBlocksPresent = True
                        , differentlyColored =
                            [ ( Sticker SecondStickerFromLeft
                              , Sticker ThirdStickerFromLeft
                              , [ Sticker ThirdStickerFromRight
                                , Sticker SecondStickerFromRight
                                ]
                              )
                            ]
                    }

                AUF.CounterClockwise ->
                    mirrorCaseSpec u


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
    , adjacentlyColored =
        List.map
            (Tuple.mapBoth
                (List.Nonempty.map mirrorElement)
                (List.Nonempty.map mirrorElement)
            )
            spec.adjacentlyColored
    , identicallyColored = List.map (mapMinLength2 mirrorElement) spec.identicallyColored
    , differentlyColored = List.map (mapMinLength2 mirrorElement) spec.differentlyColored
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
