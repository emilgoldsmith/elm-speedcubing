module Cube exposing
    ( Cube
    , solved
    , applyAlgorithm
    , viewUFRWithLetters, viewUFRNoLetters, viewUBLWithLetters
    , algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation
    )

{-|


# Definition

@docs Cube


# Constructors

@docs solved


# Modifiers

@docs applyAlgorithm


# Displayers

@docs viewUFRWithLetters, viewUFRNoLetters, viewUBLWithLetters


# Algorithm Helpers

Some algorithm functionality such as whether two algorithms are equivalent can only be determined
in the context of having a cube, so they belong here and not in the Algorithm module.

@docs algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation

-}

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


{-| Display the Up, Back and Left sides of the cube
with letters signifying the sides of the cube being
displayed on the center caps
-}
viewUBLWithLetters : List (Html.Attribute msg) -> Int -> Cube -> Html msg
viewUBLWithLetters =
    Cube.Advanced.viewUBLWithLetters


{-| Display the Up, Front and Right sides of the cube
with no letters or anything on the center caps
-}
viewUFRNoLetters : List (Html.Attribute msg) -> Int -> Cube -> Html msg
viewUFRNoLetters =
    Cube.Advanced.viewUFRNoLetters


{-| Display the Up, Front and Right sides of the cube
with letters signifying the sides of the cube being
displayed on the center caps
-}
viewUFRWithLetters : List (Html.Attribute msg) -> Int -> Cube -> Html msg
viewUFRWithLetters =
    Cube.Advanced.viewUFRWithLetters


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
