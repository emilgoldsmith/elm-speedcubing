module Cube exposing
    ( Cube
    , solved
    , applyAlgorithm
    , DisplayAngle, ufrDisplayAngle, ublDisplayAngle, dblDisplayAngle, view
    , algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation, makeAlgorithmMaintainOrientation
    , addAUFsToAlgorithm, detectAUFs
    )

{-|


# Definition

@docs Cube


# Constructors

@docs solved


# Modifiers

@docs applyAlgorithm


# Displayers

@docs DisplayAngle, ufrDisplayAngle, ublDisplayAngle, dblDisplayAngle, view


# Algorithm Helpers

Some algorithm functionality such as whether two algorithms are equivalent can only be determined
in the context of having a cube, so they belong here and not in the Algorithm module.

@docs algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation, makeAlgorithmMaintainOrientation


# AUF Helpers

Similarly to above these functions use a cube to determine their results effectively,
and therefore live here as opposed to in the AUF module

@docs addAUFsToAlgorithm, detectAUFs

-}

import AUF exposing (AUF)
import Algorithm exposing (Algorithm)
import Cube.Advanced
import Html exposing (Html)


{-| This type represents a 3x3 Rubik's Cube that can be displayed.
That means that encoded in the model is also the orientation of the whole cube.
This means that a cube with yellow facing upwards is not the same as a cube
in the same state with the green face facing upwards.

You can use the helper functions below to manipulate and display the cube. You could
for example do something like this:

    import Algorithm
    import Cube

    Cube.solved
        |> Cube.applyAlgorithm
            (
                Algorithm.fromTurnList
                    [
                        Algorithm.Turn
                            Algorithm.U
                            Algorithm.Halfway
                            Algorithm.Clockwise
                    ]
            )
        |> Cube.viewUFRNoLetters

-}
type alias Cube =
    Cube.Advanced.Cube


{-| Apply the given moves of the algorithm to the cube.
Reminder that this again depends on the orientation of the
cube how the turns will be applied, though problems with this
aren't expected to arise often given "normal use".
-}
applyAlgorithm : Algorithm -> Cube -> Cube
applyAlgorithm =
    Cube.Advanced.applyAlgorithm


{-| A solved cube with the initial orientation,
so the up face has UpColor etc.
-}
solved : Cube
solved =
    Cube.Advanced.solved


{-| The different supported angles to display a cube from
-}
type alias DisplayAngle =
    Cube.Advanced.DisplayAngle


{-| The display angle where the U, F, and R faces can be seen
-}
ufrDisplayAngle : DisplayAngle
ufrDisplayAngle =
    Cube.Advanced.ufrDisplayAngle


{-| The display angle where the U, B, and L faces can be seen
-}
ublDisplayAngle : DisplayAngle
ublDisplayAngle =
    Cube.Advanced.ublDisplayAngle


{-| The display angle where the D, B, and L faces can be seen
-}
dblDisplayAngle : DisplayAngle
dblDisplayAngle =
    Cube.Advanced.dblDisplayAngle


{-| Display a 3x3 cube from one of several angles, and with or without
the faces annotated with their respective face letters (e.g U R B).

Note that the this function uses the Html.Lazy functionality on the three record
sub-elements and on the cube. This means that in order to take advantage of this
performance improvement you must be storing the cube somewhere so that it is
equal by reference. The displayAngle should work as is as you can only access it through
the global constructors.

![Example Image Of Cube Views](https://raw.githubusercontent.com/emilgoldsmith/elm-speedcubing/7.0.1/documentation-assets/cube-views-example.png)

-}
view :
    List (Html.Attribute msg)
    ->
        { pixelSize : Int
        , displayAngle : DisplayAngle
        , annotateFaces : Bool
        }
    -> Cube
    -> Html msg
view attributes args =
    Cube.Advanced.view attributes
        { theme = Cube.Advanced.defaultTheme
        , pixelSize = args.pixelSize
        , displayAngle = args.displayAngle
        , annotateFaces = args.annotateFaces
        }


{-| Check that two algorithms produce the exact same result when
applied to a cube. They also have to end at the exact same orientation,
so while you may very rarely want this, you are probably looking for
[algorithmResultAreEquivalentIndependentOfFinalRotation](#algorithmResultsAreEquivalentIndependentOfFinalRotation)
-}
algorithmResultsAreEquivalent : Algorithm -> Algorithm -> Bool
algorithmResultsAreEquivalent =
    Cube.Advanced.algorithmResultsAreEquivalent


{-| Checks that two algorithms produce the exact same result, but they are
allowed to leave the cube in different orientations / rotations and still be considered
equivalent
-}
algorithmResultsAreEquivalentIndependentOfFinalRotation : Algorithm -> Algorithm -> Bool
algorithmResultsAreEquivalentIndependentOfFinalRotation =
    Cube.Advanced.algorithmResultsAreEquivalentIndependentOfFinalRotation


{-| Changes the algorithm so that it leaves the cube in the same orientation
that it started in if it didn't already. It doesn't change what cube state the
algorithm leaves the cube in.

If you are wondering why this doesn't live in in the Algorithm module that's a valid
question. It is because it makes the most sense in the context of the resulting cube
that comes out of an algorithm being applied. If one was to think of it without using
the Cube module one would nearly have to reimplement concepts from the Cube module.

-}
makeAlgorithmMaintainOrientation : Algorithm -> Algorithm
makeAlgorithmMaintainOrientation =
    Cube.Advanced.makeAlgorithmMaintainOrientation


{-| Add a pre and postAUF to an algorithm outputting the resulting algorithm. This
also supports algorithms that change orientation. So for example this would do the AUF
on R instead of on U for the postAUF

    import Algorithm
    import AUF

    addAUFsToAlgorithm
        ( AUF.Halfway, AUF.Clockwise )
        (Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise ])

    --> Algorithm.fromTurnList
    -->     [ Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
    -->     , Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise
    -->     , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
    -->     ]

-}
addAUFsToAlgorithm : ( AUF, AUF ) -> Algorithm -> Algorithm
addAUFsToAlgorithm =
    Cube.Advanced.addAUFsToAlgorithm


{-| Given an algorithm assumed to not have any AUFs applied to it
and an algorithm that is assumed to solve the same case as the first
algorithm but possibly have a specific AUF applied to it too,
this function figures out which AUF must be applied to the first
algorithm in order to fix the AUF-specific case the second algorithm
solves. If the function returns Nothing this means there was no AUF
combination that made the algorithms match.

Note that in case there are several AUF combinations that could solve this case
the function just returns the first one it finds.

    import Algorithm
    import AUF

    detectAUFs
        { toDetectFor = Algorithm.fromString "R" |> Result.withDefault Algorithm.empty
        , toMatchTo = Algorithm.fromString "U'RU" |> Result.withDefault Algorithm.empty
        }
    --> Just (AUF.CounterClockwise, AUF.Clockwise)

    detectAUFs
        { toDetectFor = Algorithm.fromString "R" |> Result.withDefault Algorithm.empty
        , toMatchTo = Algorithm.fromString "F" |> Result.withDefault Algorithm.empty
        }
    --> Nothing

-}
detectAUFs : { toDetectFor : Algorithm, toMatchTo : Algorithm } -> Maybe ( AUF, AUF )
detectAUFs =
    Cube.Advanced.detectAUFs
