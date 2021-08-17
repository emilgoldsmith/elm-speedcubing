module Cube exposing
    ( Cube
    , solved
    , applyAlgorithm
    , DisplayAngle, ufrDisplayAngle, ublDisplayAngle, view
    , viewAnimatable, handleAnimationMsg, animateAlgorithm, noAnimation, pauseAnimation, unpauseAnimation, AnimationState, AnimationMsg, currentTurnAnimating
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

@docs DisplayAngle, ufrDisplayAngle, ublDisplayAngle, view


## With Animation

Check out the example [on Github](https://github.com/emilgoldsmith/elm-speedcubing/blob/main/examples/src/Animation.elm) to see
how the different functions interact with each other

@docs viewAnimatable, handleAnimationMsg, animateAlgorithm, noAnimation, pauseAnimation, unpauseAnimation, AnimationState, AnimationMsg, currentTurnAnimating


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


{-| The different supported angles to display a cube from
-}
type alias DisplayAngle =
    Cube.Advanced.DisplayAngle


{-| The display angle where the U F and R faces can be seen
-}
ufrDisplayAngle : Cube.Advanced.DisplayAngle
ufrDisplayAngle =
    Cube.Advanced.ufrDisplayAngle


{-| The display angle where the U B and L faces can be seen
-}
ublDisplayAngle : Cube.Advanced.DisplayAngle
ublDisplayAngle =
    Cube.Advanced.ublDisplayAngle


{-| Display a 3x3 cube from one of several angles, and with or without
the faces annotated with their respective face letters (e.g U R B).

![Example Image Of Cube Views](https://raw.githubusercontent.com/emilgoldmith/elm-speedcubing/7.0.0/documentation-assets/cube-views-example.png)

-}
view :
    List (Html.Attribute msg)
    ->
        { pixelSize : Int
        , displayAngle : Cube.Advanced.DisplayAngle
        , annotateFaces : Bool
        }
    -> Cube
    -> Html msg
view =
    Cube.Advanced.view


{-| Display a cube that can animate turns being applied to it. You can also
specify which angle to see it from and whether to annotate the faces just like
on [view](#view)
-}
viewAnimatable :
    List (Html.Attribute msg)
    ->
        { animationState : AnimationState
        , toMsg : AnimationMsg -> msg
        , animationDoneMsg : msg
        , pixelSize : Int
        , displayAngle : DisplayAngle
        , annotateFaces : Bool
        }
    -> Cube
    -> Html msg
viewAnimatable =
    Cube.Advanced.viewAnimatable


{-| The function that can handle the opaque animation message type
and output the next animation state and any commands the package needs
to have run for it
-}
handleAnimationMsg : AnimationState -> AnimationMsg -> ( AnimationState, Cmd AnimationMsg )
handleAnimationMsg =
    Cube.Advanced.handleAnimationMsg


{-| The animation state describing the animation of
an algorithm. Set this in your model while viewAnimatable
is being used and AnimationMsg is being handled and your
cube will animate on screen
-}
animateAlgorithm : Algorithm -> AnimationState
animateAlgorithm =
    Cube.Advanced.animateAlgorithm


{-| The animation state describing a static cube with no
animated turns applied to it
-}
noAnimation : AnimationState
noAnimation =
    Cube.Advanced.noAnimation


{-| Continue a paused animation. This won't have any effect if the animation is not paused
-}
unpauseAnimation : AnimationState -> AnimationState
unpauseAnimation =
    Cube.Advanced.unpauseAnimation


{-| Pause an animation either to stop it completely or to unpause it later
-}
pauseAnimation : AnimationState -> AnimationState
pauseAnimation =
    Cube.Advanced.pauseAnimation


{-| An opaque type that describes the internal state this package uses to
manage the animation of turns on a cube
-}
type alias AnimationState =
    Cube.Advanced.AnimationState


{-| An opaque type describing the internal messages this package
uses to manage the animation of a cube
-}
type alias AnimationMsg =
    Cube.Advanced.AnimationMsg


{-| A helper function that given an animation state will tell you
which turn is currently being animated if any
-}
currentTurnAnimating : AnimationState -> Maybe Algorithm.Turn
currentTurnAnimating =
    Cube.Advanced.currentTurnAnimating


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
