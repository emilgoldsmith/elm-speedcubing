module Cube.Advanced exposing
    ( Cube
    , solved
    , applyAlgorithm
    , DisplayAngle, ufrDisplayAngle, ublDisplayAngle, dblDisplayAngle, view
    , viewAnimatable, handleAnimationMsg, animateAlgorithm, noAnimation, pauseAnimation, unpauseAnimation, currentTurnAnimating, AnimationState, AnimationMsg
    , Rendering, CubieRendering, Color(..), render
    , Face(..), UOrD(..), LOrR(..), FOrB(..), uFace, dFace, rFace, lFace, fFace, bFace, faceToColor, setColor, faces, CornerLocation, getCorner, setCorner, cornerLocations, EdgeLocation(..), getEdge, setEdge, edgeLocations, CenterLocation, getCenter, setCenter, centerLocations
    , algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation, makeAlgorithmMaintainOrientation
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


## With Animation

Check out the example [on Github](https://github.com/emilgoldsmith/elm-speedcubing/blob/main/examples/src/Animation.elm) to see
how the different functions interact with each other

@docs viewAnimatable, handleAnimationMsg, animateAlgorithm, noAnimation, pauseAnimation, unpauseAnimation, currentTurnAnimating, AnimationState, AnimationMsg


# Rendering

@docs Rendering, CubieRendering, Color, render


## Rendering Helpers

@docs Face, UOrD, LOrR, FOrB, uFace, dFace, rFace, lFace, fFace, bFace, faceToColor, setColor, faces, CornerLocation, getCorner, setCorner, cornerLocations, EdgeLocation, getEdge, setEdge, edgeLocations, CenterLocation, getCenter, setCenter, centerLocations


# Algorithm Helpers

@docs algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation, makeAlgorithmMaintainOrientation

-}

import Algorithm exposing (Algorithm)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events
import Json.Decode
import List.Nonempty
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Process
import Svg exposing (line, path, svg)
import Svg.Attributes exposing (d, fill, stroke, strokeWidth, viewBox, x1, x2, y1, y2)
import Task
import Utils.Enumerator
import Utils.MappedPermutation as MappedPermutation exposing (MappedPermutation)
import WebGL



-- CUBE MODEL


{-| See [Cube](Cube#Cube)
-}
type Cube
    = Cube CornerPositions EdgePositions CenterPositions



-- CORNER MODEL


type alias CornerPositions =
    { -- U Corners
      ufr : OrientedCorner
    , ufl : OrientedCorner
    , ubl : OrientedCorner
    , ubr : OrientedCorner

    -- D Corners
    , dfr : OrientedCorner
    , dfl : OrientedCorner
    , dbl : OrientedCorner
    , dbr : OrientedCorner
    }


type OrientedCorner
    = OrientedCorner Corner CornerOrientation


type Corner
    = -- U Corners
      UFR
    | UFL
    | UBR
    | UBL
      -- D Corners
    | DFR
    | DFL
    | DBR
    | DBL


type CornerOrientation
    = NotTwisted
    | TwistedClockwise
    | TwistedCounterClockwise



-- EDGE MODEL


type alias EdgePositions =
    { -- M Edges
      uf : OrientedEdge
    , ub : OrientedEdge
    , df : OrientedEdge
    , db : OrientedEdge

    -- S Edges
    , ur : OrientedEdge
    , ul : OrientedEdge
    , dr : OrientedEdge
    , dl : OrientedEdge

    -- E Edges
    , fr : OrientedEdge
    , fl : OrientedEdge
    , br : OrientedEdge
    , bl : OrientedEdge
    }


type OrientedEdge
    = OrientedEdge Edge EdgeOrientation


type Edge
    = -- M Edges
      UF
    | UB
    | DF
    | DB
      -- S Edges
    | UR
    | UL
    | DR
    | DL
      -- E Edges
    | FR
    | FL
    | BR
    | BL


type EdgeOrientation
    = NotFlipped
    | Flipped



-- CENTER MODEL


type alias CenterPositions =
    { u : Center
    , d : Center
    , f : Center
    , b : Center
    , l : Center
    , r : Center
    }


type Center
    = UCenter
    | DCenter
    | FCenter
    | BCenter
    | LCenter
    | RCenter



-- LOCATIONS MODEL
-- These pretty much map to the positions above, see helpers at the bottom
-- for the actual mapping


{-| Describes all the possible locations of a corner on a cube
-}
type alias CornerLocation =
    ( UOrD, FOrB, LOrR )


{-| Describes all the possible locations of an edge on a cube
-}
type EdgeLocation
    = M ( UOrD, FOrB )
    | S ( UOrD, LOrR )
    | E ( FOrB, LOrR )


{-| Describes all the possible locations of a center on a cube
-}
type CenterLocation
    = CenterLocation Face


{-| Describes all the possible faces of a cubie (a piece on the cube)
-}
type Face
    = UpOrDown UOrD
    | FrontOrBack FOrB
    | LeftOrRight LOrR


{-| A helper constructor for the U face as it's much neater to read than UpOrDown U
-}
uFace : Face
uFace =
    UpOrDown U


{-| A helper constructor for the D face as it's much neater to read than UpOrDown D
-}
dFace : Face
dFace =
    UpOrDown D


{-| A helper constructor for the F face as it's much neater to read than FrontOrBack F
-}
fFace : Face
fFace =
    FrontOrBack F


{-| A helper constructor for the B face as it's much neater to read than FrontOrBack B
-}
bFace : Face
bFace =
    FrontOrBack B


{-| A helper constructor for the L face as it's much neater to read than LeftOrRight L
-}
lFace : Face
lFace =
    LeftOrRight L


{-| A helper constructor for the R face as it's much neater to read than LeftOrRight R
-}
rFace : Face
rFace =
    LeftOrRight R


{-| A helper type that allows us to make it impossible to represent impossible locations on the cube.
By splitting the faces into opposites like we've done with these 3 types we can describe that a corner location
is exactly described by one of each of three opposing faces, and not any two faces, and similarly for edges
-}
type UOrD
    = U
    | D


{-| Same as UOrD
-}
type FOrB
    = F
    | B


{-| Same as UOrD
-}
type LOrR
    = L
    | R



-- CUBE CONSTRUCTORS


{-| See [Cube.solved](Cube#solved)
-}
solved : Cube
solved =
    let
        orientedCorner location =
            OrientedCorner location NotTwisted

        orientedEdge location =
            OrientedEdge location NotFlipped
    in
    Cube
        { -- U Corners
          ufr = orientedCorner UFR
        , ufl = orientedCorner UFL
        , ubl = orientedCorner UBL
        , ubr = orientedCorner UBR

        -- D Corners
        , dfr = orientedCorner DFR
        , dfl = orientedCorner DFL
        , dbl = orientedCorner DBL
        , dbr = orientedCorner DBR
        }
        { -- M Edges
          uf = orientedEdge UF
        , ub = orientedEdge UB
        , df = orientedEdge DF
        , db = orientedEdge DB

        -- S Edges
        , ur = orientedEdge UR
        , ul = orientedEdge UL
        , dr = orientedEdge DR
        , dl = orientedEdge DL

        -- E Edges
        , fr = orientedEdge FR
        , fl = orientedEdge FL
        , br = orientedEdge BR
        , bl = orientedEdge BL
        }
        { -- Centers
          u = UCenter
        , d = DCenter
        , f = FCenter
        , b = BCenter
        , l = LCenter
        , r = RCenter
        }



-- MOVE APPLICATION


type alias TurnDefinition =
    ( MappedPermutation CornerLocation OrientedCorner, MappedPermutation EdgeLocation OrientedEdge, MappedPermutation CenterLocation Center )


composeTurnDefinition : TurnDefinition -> TurnDefinition -> TurnDefinition
composeTurnDefinition ( corners1, edges1, centers1 ) ( corners2, edges2, centers2 ) =
    ( MappedPermutation.compose corners1 corners2
    , MappedPermutation.compose edges1 edges2
    , MappedPermutation.compose centers1 centers2
    )


{-| See [Cube.applyAlgorithm](Cube#applyAlgorithm)
-}
applyAlgorithm : Algorithm.Algorithm -> Cube -> Cube
applyAlgorithm algorithm cube =
    List.foldl applyTurn cube (Algorithm.toTurnList algorithm)


applyTurn : Algorithm.Turn -> Cube -> Cube
applyTurn =
    getTurnDefinition >> applyTurnDefinition


getTurnDefinition : Algorithm.Turn -> TurnDefinition
getTurnDefinition ((Algorithm.Turn turnable _ _) as turn) =
    turnable
        |> getClockwiseQuarterTurnDefinition
        |> toFullTurnDefinition turn


applyTurnDefinition : TurnDefinition -> Cube -> Cube
applyTurnDefinition ( cornerPermutation, edgePermutation, centerPermutation ) =
    MappedPermutation.apply (MappedPermutation.buildAccessor getCorner setCorner) cornerPermutation
        >> MappedPermutation.apply (MappedPermutation.buildAccessor getEdge setEdge) edgePermutation
        >> MappedPermutation.apply (MappedPermutation.buildAccessor getCenter setCenter) centerPermutation



-- First get the clockwise quarter turn permutation


type ClockwiseQuarterTurnDefinition
    = Permutations
        ( ClockwiseQuarterPermutation CornerLocation OrientedCorner
        , ClockwiseQuarterPermutation EdgeLocation OrientedEdge
        , ClockwiseQuarterPermutation CenterLocation Center
        )
    | Composed (List Algorithm.Turn)


type ClockwiseQuarterPermutation location cubie
    = ClockwiseQuarterPermutation (MappedPermutation location cubie)


getClockwiseQuarterTurnDefinition : Algorithm.Turnable -> ClockwiseQuarterTurnDefinition
getClockwiseQuarterTurnDefinition turnable =
    case turnable of
        -- Single face turns
        Algorithm.U ->
            buildClockwiseQuarterTurnDefinition
                [ [ ( ( U, F, R ), dontTwist )
                  , ( ( U, F, L ), dontTwist )
                  , ( ( U, B, L ), dontTwist )
                  , ( ( U, B, R ), dontTwist )
                  ]
                ]
                [ [ ( M ( U, F ), dontFlip )
                  , ( S ( U, L ), dontFlip )
                  , ( M ( U, B ), dontFlip )
                  , ( S ( U, R ), dontFlip )
                  ]
                ]
                [ noCentersMoved ]

        Algorithm.D ->
            buildClockwiseQuarterTurnDefinition
                [ [ ( ( D, F, R ), dontTwist )
                  , ( ( D, B, R ), dontTwist )
                  , ( ( D, B, L ), dontTwist )
                  , ( ( D, F, L ), dontTwist )
                  ]
                ]
                [ [ ( M ( D, F ), dontFlip )
                  , ( S ( D, R ), dontFlip )
                  , ( M ( D, B ), dontFlip )
                  , ( S ( D, L ), dontFlip )
                  ]
                ]
                [ noCentersMoved ]

        Algorithm.L ->
            buildClockwiseQuarterTurnDefinition
                [ [ ( ( U, F, L ), twistCounterClockwise )
                  , ( ( D, F, L ), twistClockwise )
                  , ( ( D, B, L ), twistCounterClockwise )
                  , ( ( U, B, L ), twistClockwise )
                  ]
                ]
                [ [ ( S ( U, L ), dontFlip )
                  , ( E ( F, L ), dontFlip )
                  , ( S ( D, L ), dontFlip )
                  , ( E ( B, L ), dontFlip )
                  ]
                ]
                [ noCentersMoved ]

        Algorithm.R ->
            buildClockwiseQuarterTurnDefinition
                [ [ ( ( U, F, R ), twistClockwise )
                  , ( ( U, B, R ), twistCounterClockwise )
                  , ( ( D, B, R ), twistClockwise )
                  , ( ( D, F, R ), twistCounterClockwise )
                  ]
                ]
                [ [ ( S ( U, R ), dontFlip )
                  , ( E ( B, R ), dontFlip )
                  , ( S ( D, R ), dontFlip )
                  , ( E ( F, R ), dontFlip )
                  ]
                ]
                [ noCentersMoved ]

        Algorithm.F ->
            buildClockwiseQuarterTurnDefinition
                [ [ ( ( U, F, L ), twistClockwise )
                  , ( ( U, F, R ), twistCounterClockwise )
                  , ( ( D, F, R ), twistClockwise )
                  , ( ( D, F, L ), twistCounterClockwise )
                  ]
                ]
                [ [ ( M ( U, F ), flipEdge )
                  , ( E ( F, R ), flipEdge )
                  , ( M ( D, F ), flipEdge )
                  , ( E ( F, L ), flipEdge )
                  ]
                ]
                [ noCentersMoved ]

        Algorithm.B ->
            buildClockwiseQuarterTurnDefinition
                [ [ ( ( U, B, R ), twistClockwise )
                  , ( ( U, B, L ), twistCounterClockwise )
                  , ( ( D, B, L ), twistClockwise )
                  , ( ( D, B, R ), twistCounterClockwise )
                  ]
                ]
                [ [ ( M ( U, B ), flipEdge )
                  , ( E ( B, L ), flipEdge )
                  , ( M ( D, B ), flipEdge )
                  , ( E ( B, R ), flipEdge )
                  ]
                ]
                [ noCentersMoved ]

        -- Slice turns
        Algorithm.M ->
            buildClockwiseQuarterTurnDefinition
                [ noCornersMoved ]
                [ [ ( M ( U, B ), flipEdge )
                  , ( M ( U, F ), flipEdge )
                  , ( M ( D, F ), flipEdge )
                  , ( M ( D, B ), flipEdge )
                  ]
                ]
                [ [ ( CenterLocation uFace, identity )
                  , ( CenterLocation fFace, identity )
                  , ( CenterLocation dFace, identity )
                  , ( CenterLocation bFace, identity )
                  ]
                ]

        Algorithm.S ->
            buildClockwiseQuarterTurnDefinition
                [ noCornersMoved ]
                [ [ ( S ( U, L ), flipEdge )
                  , ( S ( U, R ), flipEdge )
                  , ( S ( D, R ), flipEdge )
                  , ( S ( D, L ), flipEdge )
                  ]
                ]
                [ [ ( CenterLocation uFace, identity )
                  , ( CenterLocation rFace, identity )
                  , ( CenterLocation dFace, identity )
                  , ( CenterLocation lFace, identity )
                  ]
                ]

        Algorithm.E ->
            buildClockwiseQuarterTurnDefinition
                [ noCornersMoved ]
                [ [ ( E ( F, L ), flipEdge )
                  , ( E ( F, R ), flipEdge )
                  , ( E ( B, R ), flipEdge )
                  , ( E ( B, L ), flipEdge )
                  ]
                ]
                [ [ ( CenterLocation fFace, identity )
                  , ( CenterLocation rFace, identity )
                  , ( CenterLocation bFace, identity )
                  , ( CenterLocation lFace, identity )
                  ]
                ]

        Algorithm.Uw ->
            Composed
                [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.E Algorithm.OneQuarter Algorithm.CounterClockwise
                ]

        Algorithm.Dw ->
            Composed
                [ Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.E Algorithm.OneQuarter Algorithm.Clockwise
                ]

        Algorithm.Rw ->
            Composed
                [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.M Algorithm.OneQuarter Algorithm.CounterClockwise
                ]

        Algorithm.Lw ->
            Composed
                [ Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.M Algorithm.OneQuarter Algorithm.Clockwise
                ]

        Algorithm.Fw ->
            Composed
                [ Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.S Algorithm.OneQuarter Algorithm.Clockwise
                ]

        Algorithm.Bw ->
            Composed
                [ Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.S Algorithm.OneQuarter Algorithm.CounterClockwise
                ]

        Algorithm.X ->
            Composed
                [ Algorithm.Turn Algorithm.L Algorithm.OneQuarter Algorithm.CounterClockwise
                , Algorithm.Turn Algorithm.M Algorithm.OneQuarter Algorithm.CounterClockwise
                , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
                ]

        Algorithm.Y ->
            Composed
                [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.E Algorithm.OneQuarter Algorithm.CounterClockwise
                , Algorithm.Turn Algorithm.D Algorithm.OneQuarter Algorithm.CounterClockwise
                ]

        Algorithm.Z ->
            Composed
                [ Algorithm.Turn Algorithm.F Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.S Algorithm.OneQuarter Algorithm.Clockwise
                , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
                ]


buildClockwiseQuarterTurnDefinition :
    List (List ( CornerLocation, OrientedCorner -> OrientedCorner ))
    -> List (List ( EdgeLocation, OrientedEdge -> OrientedEdge ))
    -> List (List ( CenterLocation, Center -> Center ))
    -> ClockwiseQuarterTurnDefinition
buildClockwiseQuarterTurnDefinition corners edges centers =
    Permutations
        ( corners |> (MappedPermutation.build >> ClockwiseQuarterPermutation)
        , edges |> (MappedPermutation.build >> ClockwiseQuarterPermutation)
        , centers |> (MappedPermutation.build >> ClockwiseQuarterPermutation)
        )


noCentersMoved : List ( CenterLocation, Center -> Center )
noCentersMoved =
    []


noCornersMoved : List ( CornerLocation, OrientedCorner -> OrientedCorner )
noCornersMoved =
    []



-- Then use the direction and length to convert it to a full turn permutation


toFullTurnDefinition : Algorithm.Turn -> ClockwiseQuarterTurnDefinition -> TurnDefinition
toFullTurnDefinition turn quarterTurnDefinition =
    case quarterTurnDefinition of
        Permutations ( corners, edges, centers ) ->
            ( corners |> toFullPermutation turn
            , edges |> toFullPermutation turn
            , centers |> toFullPermutation turn
            )

        Composed quarterTurns ->
            let
                quarterDefinitions =
                    List.map getTurnDefinition quarterTurns

                ( quarterCorners, quarterEdges, quarterCenters ) =
                    case quarterDefinitions of
                        [] ->
                            ( MappedPermutation.identity, MappedPermutation.identity, MappedPermutation.identity )

                        x :: xs ->
                            List.foldl composeTurnDefinition x xs

                composedQuarterDefinition =
                    Permutations
                        ( ClockwiseQuarterPermutation quarterCorners
                        , ClockwiseQuarterPermutation quarterEdges
                        , ClockwiseQuarterPermutation quarterCenters
                        )
            in
            toFullTurnDefinition turn composedQuarterDefinition


toFullPermutation : Algorithm.Turn -> ClockwiseQuarterPermutation location cubie -> MappedPermutation location cubie
toFullPermutation (Algorithm.Turn _ length direction) (ClockwiseQuarterPermutation permutation) =
    case ( length, direction ) of
        ( Algorithm.OneQuarter, Algorithm.Clockwise ) ->
            permutation

        ( Algorithm.ThreeQuarters, Algorithm.CounterClockwise ) ->
            permutation

        ( Algorithm.Halfway, _ ) ->
            MappedPermutation.toThePowerOf 2 permutation

        ( Algorithm.OneQuarter, Algorithm.CounterClockwise ) ->
            MappedPermutation.reversePermutationButKeepMaps permutation

        ( Algorithm.ThreeQuarters, Algorithm.Clockwise ) ->
            MappedPermutation.reversePermutationButKeepMaps permutation



-- And here are the twists and flips


dontTwist : OrientedCorner -> OrientedCorner
dontTwist =
    identity


twistClockwise : OrientedCorner -> OrientedCorner
twistClockwise (OrientedCorner corner orientation) =
    let
        newOrientation =
            case orientation of
                NotTwisted ->
                    TwistedClockwise

                TwistedClockwise ->
                    TwistedCounterClockwise

                TwistedCounterClockwise ->
                    NotTwisted
    in
    OrientedCorner corner newOrientation


twistCounterClockwise : OrientedCorner -> OrientedCorner
twistCounterClockwise (OrientedCorner corner orientation) =
    let
        newOrientation =
            case orientation of
                NotTwisted ->
                    TwistedCounterClockwise

                TwistedCounterClockwise ->
                    TwistedClockwise

                TwistedClockwise ->
                    NotTwisted
    in
    OrientedCorner corner newOrientation


dontFlip : OrientedEdge -> OrientedEdge
dontFlip =
    identity


flipEdge : OrientedEdge -> OrientedEdge
flipEdge (OrientedEdge corner orientation) =
    let
        newOrientation =
            case orientation of
                NotFlipped ->
                    Flipped

                Flipped ->
                    NotFlipped
    in
    OrientedEdge corner newOrientation



-- Rendering Model


{-| This representation of how the cube looks is meant for advanced custom use
if you for some reason want to display the cube differently than this package
does, or you need to introspect specific state of the cube for some application.

It is expected that most use cases should be able to fulfill your goals without the
use of this, and if you find a use case that you think should be covered by the package
itself feel free to [submit a Github issue](https://github.com/emilgoldsmith/elm-speedcubing/issues/new).

Otherwise as mentioned use these rendering methods and types to accomplish all the custom
goals you want to!

-}
type alias Rendering =
    { -- U Corners
      ufr : CubieRendering
    , ufl : CubieRendering
    , ubl : CubieRendering
    , ubr : CubieRendering

    -- D Corners
    , dfr : CubieRendering
    , dfl : CubieRendering
    , dbl : CubieRendering
    , dbr : CubieRendering

    -- M Edges
    , uf : CubieRendering
    , df : CubieRendering
    , db : CubieRendering
    , ub : CubieRendering

    -- S Edges
    , ur : CubieRendering
    , ul : CubieRendering
    , dl : CubieRendering
    , dr : CubieRendering

    -- E Edges
    , fl : CubieRendering
    , fr : CubieRendering
    , br : CubieRendering
    , bl : CubieRendering

    -- Centers
    , u : CubieRendering
    , d : CubieRendering
    , f : CubieRendering
    , b : CubieRendering
    , l : CubieRendering
    , r : CubieRendering
    }


{-| The rendering of one specific cubie (a cubie is one of the pieces of the Rubik's Cube).
Note that we describe all sides of all cubies in this model, even the ones that aren't facing
outwards.
-}
type alias CubieRendering =
    { u : Color
    , d : Color
    , f : Color
    , b : Color
    , l : Color
    , r : Color
    }


{-| The color of a face of a cubie. The name of the color corresponds to
the face of the cube that it belongs to in the initial orientation. So in
standard scrambling orientation of the 3x3 cube UpColor is white and
RightColor is red. PlasticColor means it is a part of the cubie not facing
outwards
-}
type Color
    = UpColor
    | DownColor
    | FrontColor
    | BackColor
    | LeftColor
    | RightColor
    | PlasticColor


{-| Helper for easier construction of rendered cubies. This allows you to only specify
the stickers that are actually colored and the rest will automatically be designated
as PlasticColor

    ufr =
        { plainCubie | u = UpColor, f = FrontColor, r = RightColor }

-}
plainCubie : CubieRendering
plainCubie =
    { u = PlasticColor, f = PlasticColor, r = PlasticColor, d = PlasticColor, l = PlasticColor, b = PlasticColor }


{-| Get a [Rendering](#rendering) of a cube
-}
render : Cube -> Rendering
render cube =
    let
        corner =
            renderCorner cube

        edge =
            renderEdge cube

        center face =
            renderCenter cube (CenterLocation face)
    in
    { -- U Corners
      ufr = corner ( U, F, R )
    , ufl = corner ( U, F, L )
    , ubl = corner ( U, B, L )
    , ubr = corner ( U, B, R )

    -- D Corners
    , dbr = corner ( D, B, R )
    , dbl = corner ( D, B, L )
    , dfl = corner ( D, F, L )
    , dfr = corner ( D, F, R )

    -- M Edges
    , uf = edge (M ( U, F ))
    , ub = edge (M ( U, B ))
    , db = edge (M ( D, B ))
    , df = edge (M ( D, F ))

    -- S Edges
    , dl = edge (S ( D, L ))
    , dr = edge (S ( D, R ))
    , ur = edge (S ( U, R ))
    , ul = edge (S ( U, L ))

    -- E Edges
    , fl = edge (E ( F, L ))
    , fr = edge (E ( F, R ))
    , br = edge (E ( B, R ))
    , bl = edge (E ( B, L ))

    -- Centers
    , u = center uFace
    , d = center dFace
    , f = center fFace
    , b = center bFace
    , l = center lFace
    , r = center rFace
    }



-- CORNER RENDERING


{-| We have chosen the U/D layers for our reference stickers for corners. This means that
we consider a corner oriented correctly if its U or D sticker is on the U or D layer,
and respectively it is oriented clockwise / counterclockwise if the U / D sticker for
the corner is a counter / counterclockwise turn away from the U / D
-}
renderCorner : Cube -> CornerLocation -> CubieRendering
renderCorner cube location =
    let
        corner =
            getCorner location cube
    in
    plainCubie
        |> setColor (getCornerReferenceFace location) (getCornerColorOnReferenceFace corner)
        |> setColor (getClockwiseFace location) (getCornerColorOnClockwiseFace corner)
        |> setColor (getCounterClockwiseFace location) (getCornerColorOnCounterClockwiseFace corner)


getCornerColorOnReferenceFace : OrientedCorner -> Color
getCornerColorOnReferenceFace (OrientedCorner corner orientation) =
    let
        getFace =
            case orientation of
                NotTwisted ->
                    getCornerReferenceFace

                TwistedClockwise ->
                    getCounterClockwiseFace

                TwistedCounterClockwise ->
                    getClockwiseFace
    in
    corner |> getSolvedCornerLocation |> getFace |> faceToColor


getCornerColorOnClockwiseFace : OrientedCorner -> Color
getCornerColorOnClockwiseFace (OrientedCorner corner orientation) =
    let
        getFace =
            case orientation of
                NotTwisted ->
                    getClockwiseFace

                TwistedClockwise ->
                    getCornerReferenceFace

                TwistedCounterClockwise ->
                    getCounterClockwiseFace
    in
    corner |> getSolvedCornerLocation |> getFace |> faceToColor


getCornerColorOnCounterClockwiseFace : OrientedCorner -> Color
getCornerColorOnCounterClockwiseFace (OrientedCorner corner orientation) =
    let
        getFace =
            case orientation of
                NotTwisted ->
                    getCounterClockwiseFace

                TwistedClockwise ->
                    getClockwiseFace

                TwistedCounterClockwise ->
                    getCornerReferenceFace
    in
    corner |> getSolvedCornerLocation |> getFace |> faceToColor


getCornerReferenceFace : CornerLocation -> Face
getCornerReferenceFace ( uOrD, _, _ ) =
    UpOrDown uOrD


getClockwiseFace : CornerLocation -> Face
getClockwiseFace ( uOrD, fOrB, lOrR ) =
    case uOrD of
        U ->
            if isOnFLBRDiagional ( fOrB, lOrR ) then
                FrontOrBack fOrB

            else
                LeftOrRight lOrR

        D ->
            if isOnFLBRDiagional ( fOrB, lOrR ) then
                LeftOrRight lOrR

            else
                FrontOrBack fOrB


getCounterClockwiseFace : CornerLocation -> Face
getCounterClockwiseFace ( uOrD, fOrB, lOrR ) =
    case uOrD of
        U ->
            if isOnFLBRDiagional ( fOrB, lOrR ) then
                LeftOrRight lOrR

            else
                FrontOrBack fOrB

        D ->
            if isOnFLBRDiagional ( fOrB, lOrR ) then
                FrontOrBack fOrB

            else
                LeftOrRight lOrR


isOnFLBRDiagional : ( FOrB, LOrR ) -> Bool
isOnFLBRDiagional tuple =
    case tuple of
        ( F, L ) ->
            True

        ( B, R ) ->
            True

        _ ->
            False



-- EDGE RENDERING


{-| We have chosen the U/D layers for our reference stickers for edges that have a
U or D sticker. For the last 4 edges we use the F/B layers. This works the same as
corners. If the U/D F/B sticker is on the U/D F/B layer we consider the edge
oriented correctly, otherwise we consider it flipped.
-}
renderEdge : Cube -> EdgeLocation -> CubieRendering
renderEdge cube location =
    let
        edge =
            getEdge location cube
    in
    plainCubie
        |> setColor (getEdgeReferenceFace location) (getEdgeColorOnReferenceFace edge)
        |> setColor (getOtherFace location) (getEdgeColorOnOtherFace edge)


getEdgeReferenceFace : EdgeLocation -> Face
getEdgeReferenceFace location =
    case location of
        M ( uOrD, _ ) ->
            UpOrDown uOrD

        S ( uOrD, _ ) ->
            UpOrDown uOrD

        E ( fOrB, _ ) ->
            FrontOrBack fOrB


getOtherFace : EdgeLocation -> Face
getOtherFace location =
    case location of
        M ( _, fOrB ) ->
            FrontOrBack fOrB

        S ( _, lOrR ) ->
            LeftOrRight lOrR

        E ( _, lOrR ) ->
            LeftOrRight lOrR


getEdgeColorOnReferenceFace : OrientedEdge -> Color
getEdgeColorOnReferenceFace (OrientedEdge edge orientation) =
    let
        getFace =
            case orientation of
                NotFlipped ->
                    getEdgeReferenceFace

                Flipped ->
                    getOtherFace
    in
    edge |> getSolvedEdgeLocation |> getFace |> faceToColor


getEdgeColorOnOtherFace : OrientedEdge -> Color
getEdgeColorOnOtherFace (OrientedEdge edge orientation) =
    let
        getFace =
            case orientation of
                NotFlipped ->
                    getOtherFace

                Flipped ->
                    getEdgeReferenceFace
    in
    edge |> getSolvedEdgeLocation |> getFace |> faceToColor



-- CENTER RENDERING


renderCenter : Cube -> CenterLocation -> CubieRendering
renderCenter cube location =
    let
        center =
            getCenter location cube
    in
    plainCubie
        |> setColor (getCentersColoredFace location) (getCentersColor center)


getCentersColoredFace : CenterLocation -> Face
getCentersColoredFace (CenterLocation face) =
    face


getCentersColor : Center -> Color
getCentersColor =
    getSolvedCenterLocation >> getCentersColoredFace >> faceToColor



-- HELPERS - Mostly just trivial type mappings
-- Corner Location Helpers


{-| Get the corner at the given location of the given cube
-}
getCorner : CornerLocation -> Cube -> OrientedCorner
getCorner location (Cube corners _ _) =
    case location of
        ( U, F, R ) ->
            corners.ufr

        ( U, F, L ) ->
            corners.ufl

        ( U, B, R ) ->
            corners.ubr

        ( U, B, L ) ->
            corners.ubl

        ( D, B, R ) ->
            corners.dbr

        ( D, F, L ) ->
            corners.dfl

        ( D, F, R ) ->
            corners.dfr

        ( D, B, L ) ->
            corners.dbl


{-| Set the given corner at the given corner location on the given cube
-}
setCorner : CornerLocation -> OrientedCorner -> Cube -> Cube
setCorner location cornerToSet (Cube corners edges centers) =
    let
        newCorners =
            case location of
                ( U, F, R ) ->
                    { corners | ufr = cornerToSet }

                ( U, F, L ) ->
                    { corners | ufl = cornerToSet }

                ( U, B, R ) ->
                    { corners | ubr = cornerToSet }

                ( U, B, L ) ->
                    { corners | ubl = cornerToSet }

                ( D, F, R ) ->
                    { corners | dfr = cornerToSet }

                ( D, F, L ) ->
                    { corners | dfl = cornerToSet }

                ( D, B, R ) ->
                    { corners | dbr = cornerToSet }

                ( D, B, L ) ->
                    { corners | dbl = cornerToSet }
    in
    Cube newCorners edges centers


getSolvedCornerLocation : Corner -> CornerLocation
getSolvedCornerLocation corner =
    case corner of
        UFL ->
            ( U, F, L )

        UFR ->
            ( U, F, R )

        UBR ->
            ( U, B, R )

        UBL ->
            ( U, B, L )

        DFL ->
            ( D, F, L )

        DFR ->
            ( D, F, R )

        DBR ->
            ( D, B, R )

        DBL ->
            ( D, B, L )



-- Edge Location Helpers


{-| Get the edge at the given location on the given cube
-}
getEdge : EdgeLocation -> Cube -> OrientedEdge
getEdge location (Cube _ edges _) =
    case location of
        -- M Edges
        M ( U, F ) ->
            edges.uf

        M ( U, B ) ->
            edges.ub

        M ( D, F ) ->
            edges.df

        M ( D, B ) ->
            edges.db

        -- S Edges
        S ( U, R ) ->
            edges.ur

        S ( U, L ) ->
            edges.ul

        S ( D, R ) ->
            edges.dr

        S ( D, L ) ->
            edges.dl

        -- E Edges
        E ( F, R ) ->
            edges.fr

        E ( F, L ) ->
            edges.fl

        E ( B, R ) ->
            edges.br

        E ( B, L ) ->
            edges.bl


{-| Set the given edge at the given location of the given cube
-}
setEdge : EdgeLocation -> OrientedEdge -> Cube -> Cube
setEdge location edgeToSet (Cube corners edges centers) =
    let
        newEdges =
            case location of
                -- M Edges
                M ( U, F ) ->
                    { edges | uf = edgeToSet }

                M ( U, B ) ->
                    { edges | ub = edgeToSet }

                M ( D, F ) ->
                    { edges | df = edgeToSet }

                M ( D, B ) ->
                    { edges | db = edgeToSet }

                -- S Edges
                S ( U, R ) ->
                    { edges | ur = edgeToSet }

                S ( U, L ) ->
                    { edges | ul = edgeToSet }

                S ( D, R ) ->
                    { edges | dr = edgeToSet }

                S ( D, L ) ->
                    { edges | dl = edgeToSet }

                -- E Edges
                E ( F, R ) ->
                    { edges | fr = edgeToSet }

                E ( F, L ) ->
                    { edges | fl = edgeToSet }

                E ( B, R ) ->
                    { edges | br = edgeToSet }

                E ( B, L ) ->
                    { edges | bl = edgeToSet }
    in
    Cube corners newEdges centers


getSolvedEdgeLocation : Edge -> EdgeLocation
getSolvedEdgeLocation edge =
    case edge of
        -- M Edges
        UF ->
            M ( U, F )

        UB ->
            M ( U, B )

        DB ->
            M ( D, B )

        DF ->
            M ( D, F )

        -- S Edges
        UL ->
            S ( U, L )

        UR ->
            S ( U, R )

        DR ->
            S ( D, R )

        DL ->
            S ( D, L )

        -- E Edges
        FL ->
            E ( F, L )

        FR ->
            E ( F, R )

        BR ->
            E ( B, R )

        BL ->
            E ( B, L )



-- Center Location Helpers


{-| Get the center at the given location on the given cube
-}
getCenter : CenterLocation -> Cube -> Center
getCenter location (Cube _ _ centers) =
    case location of
        CenterLocation (UpOrDown U) ->
            centers.u

        CenterLocation (UpOrDown D) ->
            centers.d

        CenterLocation (FrontOrBack F) ->
            centers.f

        CenterLocation (FrontOrBack B) ->
            centers.b

        CenterLocation (LeftOrRight L) ->
            centers.l

        CenterLocation (LeftOrRight R) ->
            centers.r


{-| Set the given center at the given location on the given cube
-}
setCenter : CenterLocation -> Center -> Cube -> Cube
setCenter location center (Cube corners edges centers) =
    let
        newCenters =
            case location of
                CenterLocation (UpOrDown U) ->
                    { centers | u = center }

                CenterLocation (UpOrDown D) ->
                    { centers | d = center }

                CenterLocation (FrontOrBack F) ->
                    { centers | f = center }

                CenterLocation (FrontOrBack B) ->
                    { centers | b = center }

                CenterLocation (LeftOrRight L) ->
                    { centers | l = center }

                CenterLocation (LeftOrRight R) ->
                    { centers | r = center }
    in
    Cube corners edges newCenters


getSolvedCenterLocation : Center -> CenterLocation
getSolvedCenterLocation center =
    case center of
        UCenter ->
            CenterLocation uFace

        DCenter ->
            CenterLocation dFace

        FCenter ->
            CenterLocation fFace

        BCenter ->
            CenterLocation bFace

        LCenter ->
            CenterLocation lFace

        RCenter ->
            CenterLocation rFace



-- Rendering Helpers


{-| Set the given color on the given face of the given cubie rendering
-}
setColor : Face -> Color -> CubieRendering -> CubieRendering
setColor face color cubie =
    case face of
        UpOrDown U ->
            { cubie | u = color }

        UpOrDown D ->
            { cubie | d = color }

        FrontOrBack F ->
            { cubie | f = color }

        FrontOrBack B ->
            { cubie | b = color }

        LeftOrRight L ->
            { cubie | l = color }

        LeftOrRight R ->
            { cubie | r = color }


{-| Get the color corresponding to the given face
-}
faceToColor : Face -> Color
faceToColor face =
    case face of
        UpOrDown U ->
            UpColor

        UpOrDown D ->
            DownColor

        FrontOrBack F ->
            FrontColor

        FrontOrBack B ->
            BackColor

        LeftOrRight L ->
            LeftColor

        LeftOrRight R ->
            RightColor



-- ENUMERATORS


{-| All possible faces

    import List.Nonempty

    List.Nonempty.length faces --> 6

-}
faces : List.Nonempty.Nonempty Face
faces =
    let
        fromU face =
            case face of
                UpOrDown U ->
                    Just <| UpOrDown D

                UpOrDown D ->
                    Just <| LeftOrRight L

                LeftOrRight L ->
                    Just <| LeftOrRight R

                LeftOrRight R ->
                    Just <| FrontOrBack F

                FrontOrBack F ->
                    Just <| FrontOrBack B

                FrontOrBack B ->
                    Nothing
    in
    Utils.Enumerator.from (UpOrDown U) fromU


{-| All possible corner locations

    import List.Nonempty

    List.Nonempty.length cornerLocations --> 8

-}
cornerLocations : List.Nonempty.Nonempty CornerLocation
cornerLocations =
    let
        fromUFL location =
            case location of
                ( U, F, L ) ->
                    Just ( U, F, R )

                ( U, F, R ) ->
                    Just ( U, B, R )

                ( U, B, R ) ->
                    Just ( U, B, L )

                ( U, B, L ) ->
                    Just ( D, B, L )

                ( D, B, L ) ->
                    Just ( D, B, R )

                ( D, B, R ) ->
                    Just ( D, F, R )

                ( D, F, R ) ->
                    Just ( D, F, L )

                ( D, F, L ) ->
                    Nothing
    in
    Utils.Enumerator.from ( U, F, L ) fromUFL


{-| All possible edge locations

    import List.Nonempty

    List.Nonempty.length edgeLocations --> 12

-}
edgeLocations : List.Nonempty.Nonempty EdgeLocation
edgeLocations =
    let
        fromUF location =
            case location of
                M ( U, F ) ->
                    Just <| M ( U, B )

                M ( U, B ) ->
                    Just <| M ( D, B )

                M ( D, B ) ->
                    Just <| M ( D, F )

                M ( D, F ) ->
                    Just <| S ( U, L )

                S ( U, L ) ->
                    Just <| S ( U, R )

                S ( U, R ) ->
                    Just <| S ( D, R )

                S ( D, R ) ->
                    Just <| S ( D, L )

                S ( D, L ) ->
                    Just <| E ( F, L )

                E ( F, L ) ->
                    Just <| E ( F, R )

                E ( F, R ) ->
                    Just <| E ( B, R )

                E ( B, R ) ->
                    Just <| E ( B, L )

                E ( B, L ) ->
                    Nothing
    in
    Utils.Enumerator.from (M ( U, F )) fromUF


{-| All possible center locations

    import List.Nonempty

    List.Nonempty.length centerLocations --> 6

-}
centerLocations : List.Nonempty.Nonempty CenterLocation
centerLocations =
    let
        fromU location =
            case location of
                CenterLocation (UpOrDown U) ->
                    Just <| CenterLocation (UpOrDown D)

                CenterLocation (UpOrDown D) ->
                    Just <| CenterLocation (LeftOrRight L)

                CenterLocation (LeftOrRight L) ->
                    Just <| CenterLocation (LeftOrRight R)

                CenterLocation (LeftOrRight R) ->
                    Just <| CenterLocation (FrontOrBack F)

                CenterLocation (FrontOrBack F) ->
                    Just <| CenterLocation (FrontOrBack B)

                CenterLocation (FrontOrBack B) ->
                    Nothing
    in
    Utils.Enumerator.from (CenterLocation (UpOrDown U)) fromU



-- UI STUFF
-- Exports


{-| See [Cube.DisplayAngle](Cube#DisplayAngle)
-}
type DisplayAngle
    = UFRDisplayAngle
    | UBLDisplayAngle
    | DBLDisplayAngle


{-| See [Cube.ufrDisplayAngle](Cube#ufrDisplayAngle)
-}
ufrDisplayAngle : DisplayAngle
ufrDisplayAngle =
    UFRDisplayAngle


{-| See [Cube.ublDisplayAngle](Cube#ublDisplayAngle)
-}
ublDisplayAngle : DisplayAngle
ublDisplayAngle =
    UBLDisplayAngle


{-| See [Cube.dblDisplayAngle](Cube#dblDisplayAngle)
-}
dblDisplayAngle : DisplayAngle
dblDisplayAngle =
    DBLDisplayAngle


{-| See [Cube.view](Cube#view)
-}
view :
    List (Attribute msg)
    ->
        { pixelSize : Int
        , displayAngle : DisplayAngle
        , annotateFaces : Bool
        }
    -> Cube
    -> Html msg
view attributes { pixelSize, displayAngle, annotateFaces } cube =
    let
        { mainRotation, annotationAdjustments } =
            case displayAngle of
                UFRDisplayAngle ->
                    { mainRotation = []
                    , annotationAdjustments =
                        { u = []
                        , d = []
                        , f = []
                        , b = []
                        , l = []
                        , r = []
                        }
                    }

                UBLDisplayAngle ->
                    { mainRotation = [ YRotateDegrees 180 ]
                    , annotationAdjustments =
                        { u = [ ZRotateDegrees 180 ]
                        , d = []
                        , f = []
                        , b = []
                        , l = []
                        , r = []
                        }
                    }

                DBLDisplayAngle ->
                    { mainRotation = [ ZRotateDegrees 180, YRotateDegrees 90 ]
                    , annotationAdjustments =
                        { u = []
                        , d = [ ZRotateDegrees -90 ]
                        , f = [ ZRotateDegrees 180 ]
                        , b = [ ZRotateDegrees 180 ]
                        , l = [ ZRotateDegrees 180 ]
                        , r = [ ZRotateDegrees 180 ]
                        }
                    }
    in
    getCubeHtml attributes
        { rotation = mainRotation
        , pixelSize = pixelSize
        , annotateFaces =
            if annotateFaces then
                Just annotationAdjustments

            else
                Nothing
        , theme = defaultTheme
        }
        cube



-- PARAMETERS


type alias CubeTheme =
    { up : Rgb255Color
    , down : Rgb255Color
    , right : Rgb255Color
    , left : Rgb255Color
    , front : Rgb255Color
    , back : Rgb255Color
    , plastic : Rgb255Color
    , annotations : Rgb255Color
    }


type alias Rgb255Color =
    ( Int, Int, Int )


white : Rgb255Color
white =
    ( 200, 200, 200 )


red : Rgb255Color
red =
    ( 255, 0, 0 )


blue : Rgb255Color
blue =
    ( 0, 0, 255 )


orange : Rgb255Color
orange =
    ( 245, 121, 0 )


green : Rgb255Color
green =
    ( 0, 255, 0 )


yellow : Rgb255Color
yellow =
    ( 237, 212, 0 )


black : Rgb255Color
black =
    ( 0, 0, 0 )


defaultTheme : CubeTheme
defaultTheme =
    { up = yellow
    , down = white
    , right = orange
    , left = red
    , front = green
    , back = blue
    , plastic = black
    , annotations = black
    }



-- HTML


type alias Size =
    Int


getCubeHtml :
    List (Attribute msg)
    ->
        { rotation : Rotation
        , pixelSize : Size
        , annotateFaces :
            Maybe
                { u : Rotation
                , d : Rotation
                , l : Rotation
                , r : Rotation
                , f : Rotation
                , b : Rotation
                }
        , theme : CubeTheme
        }
    -> Cube
    -> Html msg
getCubeHtml attributes { rotation, annotateFaces, pixelSize, theme } cube =
    WebGL.toHtml
        -- All these properties including the double size of width and height
        -- are all just the suggestions in the toHtml documentation
        ([ width (pixelSize * 2)
         , height (pixelSize * 2)
         , style "width" (String.fromInt pixelSize ++ "px")
         , style "height" (String.fromInt pixelSize ++ "px")
         , style "display" "block"
         ]
            ++ attributes
        )
        (WebGL.entity
            vertexShader
            fragmentShader
            (cubeMesh theme (render cube))
            { perspective =
                perspective
            , rotation =
                rotationToWebgl rotation Mat4.identity
            }
            :: (Maybe.map (faceAnnotations theme rotation) annotateFaces
                    |> Maybe.withDefault []
               )
        )



-- WebGL stuff


type alias Vertex =
    { color : Vec3
    , position : Vec3
    , transformation : Mat4
    }


type alias Uniforms =
    { perspective : Mat4
    , rotation : Mat4
    }


perspective : Mat4
perspective =
    let
        viewportTranslation =
            Vec3.vec3 -0.2 -0.3 0

        eye =
            Vec3.vec3 0.6 0.6 1
                |> Vec3.normalize
                |> Vec3.scale 10.5
                |> Vec3.add viewportTranslation
    in
    Mat4.mul
        (Mat4.makePerspective 25 1 0.01 150)
        (Mat4.makeLookAt eye viewportTranslation Vec3.j)


{-| All this WebGL code is highly inspired by
<https://github.com/maxf/elm-webgl-rubik/tree/28f20972aee898c262461b93dbb6e45b67859b29>
-}
cubeMesh : CubeTheme -> Rendering -> WebGL.Mesh Vertex
cubeMesh theme rendering =
    List.map
        (cubieMesh theme)
        (allCubieData theme rendering)
        |> List.concat
        |> WebGL.triangles


cubieMesh : CubeTheme -> CubieData -> List ( Vertex, Vertex, Vertex )
cubieMesh theme { colors, center } =
    let
        totalCubieWidth =
            1

        borderWidth =
            0.05
    in
    [ { innerColor = colors.up
      , center = Vec3.add center (Vec3.vec3 0 (-0.5 * totalCubieWidth) 0)
      , orthogonalPlaneDirection1 = Vec3.i
      , orthogonalPlaneDirection2 = Vec3.k
      }
    , { innerColor = colors.down
      , center = Vec3.add center (Vec3.vec3 0 (0.5 * totalCubieWidth) 0)
      , orthogonalPlaneDirection1 = Vec3.i
      , orthogonalPlaneDirection2 = Vec3.k
      }
    , { innerColor = colors.front
      , center = Vec3.add center (Vec3.vec3 0 0 (0.5 * totalCubieWidth))
      , orthogonalPlaneDirection1 = Vec3.i
      , orthogonalPlaneDirection2 = Vec3.j
      }
    , { innerColor = colors.back
      , center = Vec3.add center (Vec3.vec3 0 0 (-0.5 * totalCubieWidth))
      , orthogonalPlaneDirection1 = Vec3.i
      , orthogonalPlaneDirection2 = Vec3.j
      }
    , { innerColor = colors.left
      , center = Vec3.add center (Vec3.vec3 (-0.5 * totalCubieWidth) 0 0)
      , orthogonalPlaneDirection1 = Vec3.j
      , orthogonalPlaneDirection2 = Vec3.k
      }
    , { innerColor = colors.right
      , center = Vec3.add center (Vec3.vec3 (0.5 * totalCubieWidth) 0 0)
      , orthogonalPlaneDirection1 = Vec3.j
      , orthogonalPlaneDirection2 = Vec3.k
      }
    ]
        -- Add the shared arguments
        |> List.map
            (\params ->
                { innerColor = params.innerColor
                , center = params.center
                , orthogonalPlaneDirection1 = params.orthogonalPlaneDirection1
                , orthogonalPlaneDirection2 = params.orthogonalPlaneDirection2
                , totalWidthAndHeight = totalCubieWidth
                , borderWidth = borderWidth
                , borderColor = theme.plastic |> rgb255ColorToColorVector
                }
            )
        |> List.map square
        |> List.concat


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        attribute mat4 transformation;
        uniform mat4 rotation;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * rotation * transformation * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : WebGL.Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]



-- Mesh


type alias CubieData =
    { colors : { up : Vec3, down : Vec3, front : Vec3, back : Vec3, left : Vec3, right : Vec3 }, center : Vec3 }


allCubieData : CubeTheme -> Rendering -> List CubieData
allCubieData theme rendering =
    List.map
        (\( cubieRendering, coordinates ) ->
            { center = coordinatesToCenterVector coordinates, colors = cubieRenderingToColorVectors theme cubieRendering }
        )
        (getRenderedCorners rendering
            |> List.Nonempty.append
                (getRenderedEdges rendering)
            |> List.Nonempty.append
                (getRenderedCenters rendering)
            |> List.Nonempty.toList
        )


coordinatesToCenterVector : Coordinates -> Vec3
coordinatesToCenterVector { fromFront, fromTop, fromLeft } =
    Vec3.vec3 (toFloat fromLeft - 1) (toFloat fromTop - 1) (1 - toFloat fromFront)


cubieRenderingToColorVectors : CubeTheme -> CubieRendering -> { up : Vec3, down : Vec3, front : Vec3, back : Vec3, left : Vec3, right : Vec3 }
cubieRenderingToColorVectors theme rendering =
    { up = rendering.u |> getRgb255Color theme |> rgb255ColorToColorVector
    , down = rendering.d |> getRgb255Color theme |> rgb255ColorToColorVector
    , front = rendering.f |> getRgb255Color theme |> rgb255ColorToColorVector
    , back = rendering.b |> getRgb255Color theme |> rgb255ColorToColorVector
    , left = rendering.l |> getRgb255Color theme |> rgb255ColorToColorVector
    , right = rendering.r |> getRgb255Color theme |> rgb255ColorToColorVector
    }


rgb255ColorToColorVector : Rgb255Color -> Vec3
rgb255ColorToColorVector ( x, y, z ) =
    Vec3.vec3 (toFloat x) (toFloat y) (toFloat z)


faceAnnotations :
    CubeTheme
    -> Rotation
    ->
        { u : Rotation
        , d : Rotation
        , l : Rotation
        , r : Rotation
        , f : Rotation
        , b : Rotation
        }
    -> List WebGL.Entity
faceAnnotations theme rotation adjustments =
    -- The .51 instead of just .5s are so that the annotations display in front of the cube instead of blending into the face itself
    [ WebGL.entity
        vertexShader
        fragmentShader
        (meshF theme { height = 0.6, centerPosition = Vec3.vec3 0 0 1.51, rotate = identity >> rotationToWebgl adjustments.f })
        { perspective =
            perspective
        , rotation =
            rotationToWebgl rotation Mat4.identity
        }
    , WebGL.entity
        vertexShader
        fragmentShader
        (meshL theme { height = 0.6, centerPosition = Vec3.vec3 -1.51 0 0, rotate = Mat4.rotate (degrees -90) Vec3.j >> rotationToWebgl adjustments.l })
        { perspective =
            perspective
        , rotation =
            rotationToWebgl rotation Mat4.identity
        }
    , WebGL.entity
        vertexShader
        fragmentShader
        (meshU theme { height = 0.6, centerPosition = Vec3.vec3 0 1.51 0, rotate = Mat4.rotate (degrees -90) Vec3.i >> rotationToWebgl adjustments.u })
        { perspective =
            perspective
        , rotation =
            rotationToWebgl rotation Mat4.identity
        }
    , WebGL.entity
        vertexShader
        fragmentShader
        (meshD theme { height = 0.6, centerPosition = Vec3.vec3 0 -1.51 0, rotate = Mat4.rotate (degrees 90) Vec3.i >> rotationToWebgl adjustments.d })
        { perspective =
            perspective
        , rotation =
            rotationToWebgl rotation Mat4.identity
        }
    , WebGL.entity
        vertexShader
        fragmentShader
        (meshR theme { height = 0.6, centerPosition = Vec3.vec3 1.51 0 0, rotate = Mat4.rotate (degrees 90) Vec3.j >> rotationToWebgl adjustments.r })
        { perspective =
            perspective
        , rotation =
            rotationToWebgl rotation Mat4.identity
        }
    , WebGL.entity
        vertexShader
        fragmentShader
        (meshB theme { height = 0.6, centerPosition = Vec3.vec3 0 0 -1.51, rotate = Mat4.rotate (degrees 180) Vec3.j >> rotationToWebgl adjustments.b })
        { perspective =
            perspective
        , rotation =
            rotationToWebgl rotation Mat4.identity
        }
    ]


rotationToWebgl : Rotation -> Mat4 -> Mat4
rotationToWebgl rotation =
    List.foldl
        (\singleTransform currentTransform ->
            case singleTransform of
                XRotateDegrees deg ->
                    Mat4.rotate (degrees deg) Vec3.i << currentTransform

                YRotateDegrees deg ->
                    Mat4.rotate (degrees deg) Vec3.j << currentTransform

                ZRotateDegrees deg ->
                    Mat4.rotate (degrees deg) Vec3.k << currentTransform
        )
        identity
        rotation



-- LOGIC AND MAPPINGS


getRgb255Color : CubeTheme -> Color -> Rgb255Color
getRgb255Color theme color =
    case color of
        UpColor ->
            theme.up

        DownColor ->
            theme.down

        RightColor ->
            theme.right

        LeftColor ->
            theme.left

        FrontColor ->
            theme.front

        BackColor ->
            theme.back

        PlasticColor ->
            theme.plastic


{-| We only use ints for now so it makes some things a bit easier
but there's no real reason other than simpler code a few places that they
can't be floats
-}
type alias Coordinates =
    { fromFront : Int
    , fromLeft : Int
    , fromTop : Int
    }


type SingleRotation
    = XRotateDegrees Float
    | YRotateDegrees Float
    | ZRotateDegrees Float


{-| 3D Rotation. Note rotations are applied from left to right
-}
type alias Rotation =
    List SingleRotation


getRenderedCorners : Rendering -> List.Nonempty.Nonempty ( CubieRendering, Coordinates )
getRenderedCorners rendering =
    List.Nonempty.map (getRenderedCorner rendering) cornerLocations


getRenderedCorner : Rendering -> CornerLocation -> ( CubieRendering, Coordinates )
getRenderedCorner rendering location =
    let
        cornerRendering =
            case location of
                ( U, F, L ) ->
                    rendering.ufl

                ( U, F, R ) ->
                    rendering.ufr

                ( U, B, R ) ->
                    rendering.ubr

                ( U, B, L ) ->
                    rendering.ubl

                ( D, B, L ) ->
                    rendering.dbl

                ( D, B, R ) ->
                    rendering.dbr

                ( D, F, R ) ->
                    rendering.dfr

                ( D, F, L ) ->
                    rendering.dfl
    in
    ( cornerRendering, getCornerCoordinates location )


getCornerCoordinates : CornerLocation -> Coordinates
getCornerCoordinates ( uOrD, fOrB, lOrR ) =
    { fromFront =
        if fOrB == F then
            0

        else
            2
    , fromLeft =
        if lOrR == L then
            0

        else
            2
    , fromTop =
        if uOrD == U then
            0

        else
            2
    }


getRenderedEdges : Rendering -> List.Nonempty.Nonempty ( CubieRendering, Coordinates )
getRenderedEdges rendering =
    List.Nonempty.map (getRenderedEdge rendering) edgeLocations


getRenderedEdge : Rendering -> EdgeLocation -> ( CubieRendering, Coordinates )
getRenderedEdge rendering location =
    let
        edgeRendering =
            case location of
                M ( U, F ) ->
                    rendering.uf

                M ( U, B ) ->
                    rendering.ub

                M ( D, F ) ->
                    rendering.df

                M ( D, B ) ->
                    rendering.db

                S ( U, L ) ->
                    rendering.ul

                S ( U, R ) ->
                    rendering.ur

                S ( D, L ) ->
                    rendering.dl

                S ( D, R ) ->
                    rendering.dr

                E ( F, L ) ->
                    rendering.fl

                E ( F, R ) ->
                    rendering.fr

                E ( B, L ) ->
                    rendering.bl

                E ( B, R ) ->
                    rendering.br
    in
    ( edgeRendering, getEdgeCoordinates location )


getEdgeCoordinates : EdgeLocation -> Coordinates
getEdgeCoordinates location =
    case location of
        M ( uOrD, fOrB ) ->
            { fromFront =
                if fOrB == F then
                    0

                else
                    2
            , fromLeft = 1
            , fromTop =
                if uOrD == U then
                    0

                else
                    2
            }

        S ( uOrD, lOrR ) ->
            { fromFront = 1
            , fromLeft =
                if lOrR == L then
                    0

                else
                    2
            , fromTop =
                if uOrD == U then
                    0

                else
                    2
            }

        E ( fOrB, lOrR ) ->
            { fromFront =
                if fOrB == F then
                    0

                else
                    2
            , fromLeft =
                if lOrR == L then
                    0

                else
                    2
            , fromTop = 1
            }


getRenderedCenters : Rendering -> List.Nonempty.Nonempty ( CubieRendering, Coordinates )
getRenderedCenters rendering =
    List.Nonempty.map (getRenderedCenter rendering) centerLocations


getRenderedCenter : Rendering -> CenterLocation -> ( CubieRendering, Coordinates )
getRenderedCenter rendering location =
    let
        centerRendering =
            case location of
                CenterLocation (UpOrDown U) ->
                    rendering.u

                CenterLocation (UpOrDown D) ->
                    rendering.d

                CenterLocation (LeftOrRight L) ->
                    rendering.l

                CenterLocation (LeftOrRight R) ->
                    rendering.r

                CenterLocation (FrontOrBack F) ->
                    rendering.f

                CenterLocation (FrontOrBack B) ->
                    rendering.b
    in
    ( centerRendering
    , getCenterCoordinates location
    )


meshF : CubeTheme -> { height : Float, centerPosition : Vec3, rotate : Mat4 -> Mat4 } -> WebGL.Mesh Vertex
meshF theme { height, centerPosition, rotate } =
    let
        -- Bounding box pre-scaling is 150 (width) x 225 (height)
        boundingWidth =
            150

        boundingHeight =
            225

        width =
            height * boundingWidth / boundingHeight

        stemWidth =
            30

        branchesWidth =
            25
    in
    [ triangleLine
        { from = Vec2.vec2 (stemWidth / 2) 0
        , to = Vec2.vec2 (stemWidth / 2) boundingHeight
        , zCoordinate = 0
        , width = stemWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 0 (boundingHeight - branchesWidth / 2)
        , to = Vec2.vec2 boundingWidth (boundingHeight - branchesWidth / 2)
        , zCoordinate = 0
        , width = branchesWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 0 (boundingHeight / 2)
        , to = Vec2.vec2 (boundingWidth * 13 / 15) (boundingHeight / 2)
        , zCoordinate = 0
        , width = branchesWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    ]
        |> List.concat
        |> List.map (mapTriple <| mapPosition <| Vec3.scale <| height / boundingHeight)
        |> List.map
            (mapTriple <|
                setTransformation
                    (Mat4.identity
                        |> Mat4.translate centerPosition
                        |> rotate
                        |> Mat4.translate3 -(width / 2) -(height / 2) 0
                    )
            )
        |> WebGL.triangles


noTransformationVertex : { color : Vec3, position : Vec3 } -> Vertex
noTransformationVertex { color, position } =
    { color = color
    , position = position
    , transformation = Mat4.identity
    }


setTransformation : Mat4 -> Vertex -> Vertex
setTransformation newTransformation oldVertex =
    { oldVertex | transformation = newTransformation }


mapPosition : (Vec3 -> Vec3) -> Vertex -> Vertex
mapPosition fn original =
    { original | position = fn original.position }


twoDTrianglesToColored3d : { zCoordinate : Float, color : Vec3 } -> List ( Vec2, Vec2, Vec2 ) -> List ( Vertex, Vertex, Vertex )
twoDTrianglesToColored3d { zCoordinate, color } triangles =
    triangles
        |> List.map (mapTriple <| twoDTo3d zCoordinate)
        |> List.map (mapTriple <| positionToVertex { color = color })


twoDTo3d : Float -> Vec2 -> Vec3
twoDTo3d zCoordinate xy =
    Vec3.vec3 (Vec2.getX xy) (Vec2.getY xy) zCoordinate


positionToVertex : { color : Vec3 } -> Vec3 -> Vertex
positionToVertex { color } position =
    { position = position, color = color, transformation = Mat4.identity }


mapTriple : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTriple fn ( x, y, z ) =
    ( fn x, fn y, fn z )


meshL : CubeTheme -> { height : Float, centerPosition : Vec3, rotate : Mat4 -> Mat4 } -> WebGL.Mesh Vertex
meshL theme { height, centerPosition, rotate } =
    let
        -- Bounding box pre-scaling is 150 (width) x 225 (height)
        boundingWidth =
            150

        boundingHeight =
            225

        width =
            height * boundingWidth / boundingHeight

        stemWidth =
            30

        branchesWidth =
            25
    in
    [ triangleLine
        { from = Vec2.vec2 (stemWidth / 2) 0
        , to = Vec2.vec2 (stemWidth / 2) boundingHeight
        , zCoordinate = 0
        , width = stemWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 0 (branchesWidth / 2)
        , to = Vec2.vec2 boundingWidth (branchesWidth / 2)
        , zCoordinate = 0
        , width = branchesWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    ]
        |> List.concat
        |> List.map (mapTriple <| mapPosition <| Vec3.scale <| height / boundingHeight)
        |> List.map
            (mapTriple <|
                setTransformation
                    (Mat4.identity
                        |> Mat4.translate centerPosition
                        |> rotate
                        |> Mat4.translate3 -(width / 2) -(height / 2) 0
                    )
            )
        |> WebGL.triangles


meshU : CubeTheme -> { height : Float, centerPosition : Vec3, rotate : Mat4 -> Mat4 } -> WebGL.Mesh Vertex
meshU theme { height, centerPosition, rotate } =
    let
        -- Bounding box pre-scaling is 220 (width) x 300 (height)
        boundingWidth =
            220

        boundingHeight =
            300

        width =
            height * boundingWidth / boundingHeight

        strokeWidth =
            35
    in
    [ triangleLine
        { from = Vec2.vec2 (strokeWidth / 2) 0
        , to = Vec2.vec2 (strokeWidth / 2) (boundingHeight * 2 / 3)
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , halfEllipse
        { startX = strokeWidth / 2
        , endX = boundingWidth - strokeWidth / 2
        , centerYCoordinate = boundingHeight * 2 / 3
        , zCoordinate = 0
        , height = 82.5
        , granularity = 10
        , strokeWidth = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 (boundingWidth - strokeWidth / 2) 0
        , to = Vec2.vec2 (boundingWidth - strokeWidth / 2) (boundingHeight * 2 / 3)
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    ]
        |> List.concat
        |> List.map (mapTriple <| mapPosition <| Vec3.scale <| height / boundingHeight)
        |> List.map
            (mapTriple <|
                setTransformation
                    (Mat4.identity
                        |> Mat4.translate centerPosition
                        |> rotate
                        |> Mat4.rotate (degrees 180) Vec3.k
                        |> Mat4.translate3 -(width / 2) -(height / 2) 0
                    )
            )
        |> WebGL.triangles


square :
    { center : Vec3
    , innerColor : Vec3
    , borderColor : Vec3
    , orthogonalPlaneDirection1 : Vec3
    , orthogonalPlaneDirection2 : Vec3
    , totalWidthAndHeight : Float
    , borderWidth : Float
    }
    -> List ( Vertex, Vertex, Vertex )
square { center, innerColor, orthogonalPlaneDirection1, orthogonalPlaneDirection2, totalWidthAndHeight, borderWidth, borderColor } =
    let
        innerVertex position =
            { color = Vec3.scale (1 / 255) innerColor, position = position }

        borderVertex position =
            { color = Vec3.scale (1 / 255) borderColor, position = position }

        innerWidthAndHeight =
            totalWidthAndHeight - 2 * borderWidth

        addHalfWidthInDirection width direction point =
            direction
                |> Vec3.normalize
                |> Vec3.scale (width / 2)
                |> Vec3.add point

        -- Inner Corners
        { a, b, c, d } =
            { a =
                center
                    -- The scale by 1 are just for readability of the -1 and 1 difference
                    |> addHalfWidthInDirection innerWidthAndHeight (Vec3.scale 1 orthogonalPlaneDirection1)
                    |> addHalfWidthInDirection innerWidthAndHeight (Vec3.scale 1 orthogonalPlaneDirection2)
            , b =
                center
                    |> addHalfWidthInDirection innerWidthAndHeight (Vec3.scale -1 orthogonalPlaneDirection1)
                    |> addHalfWidthInDirection innerWidthAndHeight (Vec3.scale 1 orthogonalPlaneDirection2)
            , c =
                center
                    |> addHalfWidthInDirection innerWidthAndHeight (Vec3.scale -1 orthogonalPlaneDirection1)
                    |> addHalfWidthInDirection innerWidthAndHeight (Vec3.scale -1 orthogonalPlaneDirection2)
            , d =
                center
                    |> addHalfWidthInDirection innerWidthAndHeight (Vec3.scale 1 orthogonalPlaneDirection1)
                    |> addHalfWidthInDirection innerWidthAndHeight (Vec3.scale -1 orthogonalPlaneDirection2)
            }

        -- Corresponding Outer Corners
        { aa, bb, cc, dd } =
            { aa =
                center
                    -- The scale by 1 are just for readability of the -1 and 1 difference
                    |> addHalfWidthInDirection totalWidthAndHeight (Vec3.scale 1 orthogonalPlaneDirection1)
                    |> addHalfWidthInDirection totalWidthAndHeight (Vec3.scale 1 orthogonalPlaneDirection2)
            , bb =
                center
                    |> addHalfWidthInDirection totalWidthAndHeight (Vec3.scale -1 orthogonalPlaneDirection1)
                    |> addHalfWidthInDirection totalWidthAndHeight (Vec3.scale 1 orthogonalPlaneDirection2)
            , cc =
                center
                    |> addHalfWidthInDirection totalWidthAndHeight (Vec3.scale -1 orthogonalPlaneDirection1)
                    |> addHalfWidthInDirection totalWidthAndHeight (Vec3.scale -1 orthogonalPlaneDirection2)
            , dd =
                center
                    |> addHalfWidthInDirection totalWidthAndHeight (Vec3.scale 1 orthogonalPlaneDirection1)
                    |> addHalfWidthInDirection totalWidthAndHeight (Vec3.scale -1 orthogonalPlaneDirection2)
            }
    in
    [ ( innerVertex a, innerVertex b, innerVertex c )
    , ( innerVertex c, innerVertex d, innerVertex a )
    , ( borderVertex aa, borderVertex bb, borderVertex b )
    , ( borderVertex b, borderVertex a, borderVertex aa )
    , ( borderVertex bb, borderVertex cc, borderVertex c )
    , ( borderVertex c, borderVertex b, borderVertex bb )
    , ( borderVertex cc, borderVertex dd, borderVertex d )
    , ( borderVertex d, borderVertex c, borderVertex cc )
    , ( borderVertex dd, borderVertex aa, borderVertex a )
    , ( borderVertex a, borderVertex d, borderVertex dd )
    ]
        |> List.map (mapTriple noTransformationVertex)


triangleLine : { from : Vec2, to : Vec2, zCoordinate : Float, width : Float, color : Vec3 } -> List ( Vertex, Vertex, Vertex )
triangleLine { from, to, width, color, zCoordinate } =
    let
        diff =
            Vec2.sub to from

        normal =
            Vec2.vec2 -(Vec2.getY diff) (Vec2.getX diff)
                |> Vec2.normalize

        halfWidthLengthNormal =
            Vec2.scale (width / 2) normal

        a =
            Vec2.add from halfWidthLengthNormal

        b =
            Vec2.add a diff

        c =
            Vec2.sub b (Vec2.scale 2 halfWidthLengthNormal)

        d =
            Vec2.sub c diff
    in
    [ ( a, b, c ), ( c, a, d ) ]
        |> twoDTrianglesToColored3d { zCoordinate = zCoordinate, color = color }


{-| Granularity is the length of each line segment in the curve
-}
halfEllipse :
    { height : Float
    , centerYCoordinate : Float
    , zCoordinate : Float
    , startX : Float
    , endX : Float
    , granularity : Float
    , strokeWidth : Float
    , color : Vec3
    }
    -> List ( Vertex, Vertex, Vertex )
halfEllipse params =
    let
        width =
            params.endX - params.startX

        center =
            Vec3.vec3 (params.startX + width / 2) params.centerYCoordinate params.zCoordinate
    in
    halfEllipseHelper
        { width = width
        , height = params.height
        , granularity = params.granularity
        , strokeWidth = params.strokeWidth
        , color = params.color
        }
        { x = -width / 2, triangles = [] }
        |> addBeginningEllipseLine
            { startX = -width / 2
            , startY = 0
            , width = params.strokeWidth
            , color = params.color
            }
        |> addEndingEllipseLine
            { startX = width / 2
            , startY = 0
            , width = params.strokeWidth
            , color = params.color
            }
        |> List.map (mapTriple <| mapPosition <| Vec3.add center)


halfEllipseHelper : { width : Float, height : Float, granularity : Float, strokeWidth : Float, color : Vec3 } -> { x : Float, triangles : List ( Vertex, Vertex, Vertex ) } -> List ( Vertex, Vertex, Vertex )
halfEllipseHelper params { x, triangles } =
    let
        rx =
            params.width / 2

        ry =
            params.height

        startCoordinates =
            getPositiveEllipseCoordinatesFromX { rx = rx, ry = ry } x

        endCoordinates =
            getNextCoordinates { rx = rx, ry = ry, granularity = params.granularity } startCoordinates

        newLineSegment =
            triangleLine { from = startCoordinates, to = endCoordinates, width = params.strokeWidth, zCoordinate = 0, color = params.color }
    in
    if rx - Vec2.getX endCoordinates < params.granularity / 20 then
        triangles ++ newLineSegment

    else
        halfEllipseHelper params { x = Vec2.getX endCoordinates, triangles = triangles ++ newLineSegment }


getNextCoordinates : { rx : Float, ry : Float, granularity : Float } -> Vec2 -> Vec2
getNextCoordinates { rx, ry, granularity } current =
    binarySearchEllipseDistance
        { rx = rx
        , ry = ry
        , targetDistance = granularity
        , minX = Vec2.getX current
        , maxX = Basics.min rx (Vec2.getX current + granularity)
        , startPoint = current
        }


binarySearchEllipseDistance : { rx : Float, ry : Float, targetDistance : Float, minX : Float, maxX : Float, startPoint : Vec2 } -> Vec2
binarySearchEllipseDistance ({ rx, ry, targetDistance, minX, maxX, startPoint } as params) =
    let
        xToTest =
            (minX + maxX) / 2

        testEndPoint =
            getPositiveEllipseCoordinatesFromX { rx = rx, ry = ry } xToTest

        testDistance =
            Vec2.distance startPoint testEndPoint
    in
    if testDistance - targetDistance < targetDistance / 10 then
        testEndPoint

    else if maxX - minX < targetDistance / 10 then
        getPositiveEllipseCoordinatesFromX { rx = rx, ry = ry } maxX

    else if testDistance >= targetDistance then
        binarySearchEllipseDistance { params | maxX = xToTest }

    else
        binarySearchEllipseDistance { params | minX = xToTest }


getPositiveEllipseCoordinatesFromX : { rx : Float, ry : Float } -> Float -> Vec2
getPositiveEllipseCoordinatesFromX { rx, ry } x =
    Vec2.fromRecord
        { x = x
        , y = ry * sqrt (rx * rx - x * x) / rx
        }


addBeginningEllipseLine : { startX : Float, startY : Float, width : Float, color : Vec3 } -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
addBeginningEllipseLine { startX, startY, width, color } vertices =
    case vertices of
        first :: second :: _ ->
            let
                firstLineCorners =
                    List.Nonempty.Nonempty first [ second ]
                        |> List.Nonempty.map (\( a, b, c ) -> List.Nonempty.Nonempty a [ b, c ])
                        |> List.Nonempty.concat

                minXVertex =
                    firstLineCorners
                        |> List.Nonempty.sortBy (.position >> Vec3.getX)
                        |> List.Nonempty.head

                beginningLine =
                    triangleLine { from = Vec2.vec2 startX startY, to = Vec2.vec2 startX (Vec3.getY minXVertex.position), zCoordinate = 0, width = width, color = color }
            in
            beginningLine ++ vertices

        _ ->
            vertices


addEndingEllipseLine : { startX : Float, startY : Float, width : Float, color : Vec3 } -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
addEndingEllipseLine params vertices =
    vertices
        |> negateAllXCoordinates
        |> List.reverse
        |> addBeginningEllipseLine { params | startX = -params.startX }
        |> negateAllXCoordinates
        |> List.reverse


negateAllXCoordinates : List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
negateAllXCoordinates =
    List.map (mapTriple <| mapPosition (\pos -> Vec3.setX (-1 * Vec3.getX pos) pos))


meshD : CubeTheme -> { height : Float, centerPosition : Vec3, rotate : Mat4 -> Mat4 } -> WebGL.Mesh Vertex
meshD theme { height, centerPosition, rotate } =
    let
        -- Bounding box pre-scaling and rotation is 290 (width) x 230 (height)
        -- and for ease of ellipse use we are drawing the D "lying down"
        boundingWidth =
            290

        boundingHeight =
            200

        -- Since we'll be doing some rotation that will actually swap
        -- the width and height
        preRotateWidth =
            height

        preRotateHeight =
            preRotateWidth * boundingHeight / boundingWidth

        strokeWidth =
            35
    in
    [ triangleLine
        { from = Vec2.vec2 0 (strokeWidth / 2)
        , to = Vec2.vec2 boundingWidth (strokeWidth / 2)
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , halfEllipse
        { startX = strokeWidth / 2
        , endX = boundingWidth - strokeWidth / 2
        , centerYCoordinate = strokeWidth
        , zCoordinate = 0
        , height = boundingHeight - strokeWidth * 3 / 2
        , granularity = 10
        , strokeWidth = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    ]
        |> List.concat
        |> List.map (mapTriple <| mapPosition <| Vec3.scale <| preRotateHeight / boundingHeight)
        |> List.map
            (mapTriple <|
                setTransformation
                    (Mat4.identity
                        |> Mat4.translate centerPosition
                        |> rotate
                        |> Mat4.rotate (degrees -90) Vec3.k
                        |> Mat4.translate3 -(preRotateWidth / 2) -(preRotateHeight / 2) 0
                    )
            )
        |> WebGL.triangles


meshR : CubeTheme -> { height : Float, centerPosition : Vec3, rotate : Mat4 -> Mat4 } -> WebGL.Mesh Vertex
meshR theme { height, centerPosition, rotate } =
    let
        -- Bounding box pre-scaling and rotation is 290 (width) x 255 (height)
        -- and for ease of ellipse use we are drawing the R "lying down"
        boundingWidth =
            290

        boundingHeight =
            210

        -- Since we'll be doing some rotation that will actually swap
        -- the width and height
        preRotateWidth =
            height

        preRotateHeight =
            preRotateWidth * boundingHeight / boundingWidth

        strokeWidth =
            35
    in
    [ triangleLine
        { from = Vec2.vec2 0 (strokeWidth / 2)
        , to = Vec2.vec2 boundingWidth (strokeWidth / 2)
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 (strokeWidth / 2) 0
        , to = Vec2.vec2 (strokeWidth / 2) (boundingHeight * 10 / 21)
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , halfEllipse
        { startX = strokeWidth / 2
        , endX = boundingWidth / 2
        , centerYCoordinate = boundingHeight * 10 / 21
        , zCoordinate = 0
        , height = boundingHeight * 95 / 210
        , granularity = 10
        , strokeWidth = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 (boundingWidth / 2) (boundingHeight * 10 / 21)
        , to = Vec2.vec2 (boundingWidth / 2) 0
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 (boundingWidth / 2) (boundingHeight * 12 / 21)
        , to = Vec2.vec2 boundingWidth (boundingWidth * 2 / 3)
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    ]
        |> List.concat
        |> List.map (mapTriple <| mapPosition <| Vec3.scale <| preRotateHeight / boundingHeight)
        |> List.map
            (mapTriple <|
                setTransformation
                    (Mat4.identity
                        |> Mat4.translate centerPosition
                        |> rotate
                        |> Mat4.rotate (degrees -90) Vec3.k
                        |> Mat4.translate3 -(preRotateWidth / 2) -(preRotateHeight / 2) 0
                    )
            )
        |> WebGL.triangles


meshB : CubeTheme -> { height : Float, centerPosition : Vec3, rotate : Mat4 -> Mat4 } -> WebGL.Mesh Vertex
meshB theme { height, centerPosition, rotate } =
    let
        -- Bounding box pre-scaling and rotation is 290 (width) x 230 (height)
        -- and for ease of ellipse use we are drawing the B "lying down"
        boundingWidth =
            290

        boundingHeight =
            205

        -- Since we'll be doing some rotation that will actually swap
        -- the width and height
        preRotateWidth =
            height

        preRotateHeight =
            preRotateWidth * boundingHeight / boundingWidth

        strokeWidth =
            35
    in
    [ triangleLine
        { from = Vec2.vec2 0 (strokeWidth / 2)
        , to = Vec2.vec2 boundingWidth (strokeWidth / 2)
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 (strokeWidth / 2) 0
        , to = Vec2.vec2 (strokeWidth / 2) (boundingHeight * 10 / 23)
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , halfEllipse
        { startX = strokeWidth / 2
        , endX = boundingWidth / 2
        , centerYCoordinate = boundingHeight * 10 / 23
        , zCoordinate = 0
        , height = boundingHeight * 95 / 255
        , granularity = 10
        , strokeWidth = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 (boundingWidth / 2) (boundingHeight * 10 / 23)
        , to = Vec2.vec2 (boundingWidth / 2) 0
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , halfEllipse
        { startX = boundingWidth / 2
        , endX = boundingWidth - strokeWidth / 2
        , centerYCoordinate = boundingHeight * 10 / 23
        , zCoordinate = 0
        , height = boundingHeight * 95 / 255
        , granularity = 10
        , strokeWidth = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    , triangleLine
        { from = Vec2.vec2 (boundingWidth - strokeWidth / 2) (boundingHeight * 10 / 23)
        , to = Vec2.vec2 (boundingWidth - strokeWidth / 2) 0
        , zCoordinate = 0
        , width = strokeWidth
        , color = theme.annotations |> rgb255ColorToColorVector
        }
    ]
        |> List.concat
        |> List.map (mapTriple <| mapPosition <| Vec3.scale <| preRotateHeight / boundingHeight)
        |> List.map
            (mapTriple <|
                setTransformation
                    (Mat4.identity
                        |> Mat4.translate centerPosition
                        |> rotate
                        |> Mat4.rotate (degrees -90) Vec3.k
                        |> Mat4.translate3 -(preRotateWidth / 2) -(preRotateHeight / 2) 0
                    )
            )
        |> WebGL.triangles


getCenterCoordinates : CenterLocation -> Coordinates
getCenterCoordinates location =
    case location of
        CenterLocation (UpOrDown U) ->
            { fromFront = 1
            , fromLeft = 1
            , fromTop = 0
            }

        CenterLocation (UpOrDown D) ->
            { fromFront = 1
            , fromLeft = 1
            , fromTop = 2
            }

        CenterLocation (LeftOrRight L) ->
            { fromFront = 1
            , fromLeft = 0
            , fromTop = 1
            }

        CenterLocation (LeftOrRight R) ->
            { fromFront = 1
            , fromLeft = 2
            , fromTop = 1
            }

        CenterLocation (FrontOrBack F) ->
            { fromFront = 0
            , fromLeft = 1
            , fromTop = 1
            }

        CenterLocation (FrontOrBack B) ->
            { fromFront = 2
            , fromLeft = 1
            , fromTop = 1
            }


{-| See [Cube.algorithmResultsAreEquivalent](Cube#algorithmResultsAreEquivalent)
-}
algorithmResultsAreEquivalent : Algorithm -> Algorithm -> Bool
algorithmResultsAreEquivalent a b =
    solved
        |> applyAlgorithm a
        |> applyAlgorithm (Algorithm.inverse b)
        |> (==) solved


{-| See [Cube.algorithmResultsAreEquivalentIndependentOfFinalRotation](Cube#algorithmResultsAreEquivalentIndependentOfRotation)
-}
algorithmResultsAreEquivalentIndependentOfFinalRotation : Algorithm -> Algorithm -> Bool
algorithmResultsAreEquivalentIndependentOfFinalRotation a b =
    algorithmResultsAreEquivalent
        (makeAlgorithmMaintainOrientation a)
        (makeAlgorithmMaintainOrientation b)


{-| See [Cube.makeAlgorithmMaintainOrientation](Cube#makeAlgorithmMaintainOrientation)
-}
makeAlgorithmMaintainOrientation : Algorithm -> Algorithm
makeAlgorithmMaintainOrientation algorithm =
    let
        faceToMoveToU =
            findFaceWithCenterColor
                UpColor
                (render <| applyAlgorithm algorithm solved)

        allYRotations =
            Algorithm.allTurnLengths
                |> List.Nonempty.map (\length -> Algorithm.Turn Algorithm.Y length Algorithm.Clockwise)
                |> List.Nonempty.map (List.singleton >> Algorithm.fromTurnList)
                |> List.Nonempty.cons Algorithm.empty
    in
    algorithm
        -- We first fix the U (and D) face
        |> rotateSoFaceIsOnU faceToMoveToU
        -- Now we should be able to use a y-axis rotation to fix the last 4 faces
        |> (\uFixedAlgorithm ->
                let
                    yRotationPossibilities =
                        List.Nonempty.map
                            (Algorithm.append uFixedAlgorithm)
                            allYRotations
                in
                List.Nonempty.filter
                    hasStartingOrientation
                    uFixedAlgorithm
                    yRotationPossibilities
           )
        -- We use a default with the filter etc. above, so it is definitely important
        -- this code has some good tests to ensure confidence in the logic
        |> List.Nonempty.head


findFaceWithCenterColor : Color -> Rendering -> Face
findFaceWithCenterColor color rendering =
    List.Nonempty.map (\face -> ( centerColorOnFace face rendering, face )) faces
        -- We trust the tests here by using the default this nonempty filter requires
        -- as this case should never happen but in case it does good tests hopefully catch it
        |> List.Nonempty.filter (Tuple.first >> (==) color) ( UpColor, UpOrDown U )
        |> List.Nonempty.head
        |> Tuple.second


centerColorOnFace : Face -> Rendering -> Color
centerColorOnFace face rendering =
    case face of
        UpOrDown U ->
            rendering.u.u

        UpOrDown D ->
            rendering.d.d

        LeftOrRight R ->
            rendering.r.r

        LeftOrRight L ->
            rendering.l.l

        FrontOrBack F ->
            rendering.f.f

        FrontOrBack B ->
            rendering.b.b


rotateSoFaceIsOnU : Face -> Algorithm -> Algorithm
rotateSoFaceIsOnU face algorithm =
    Algorithm.append algorithm <|
        case face of
            UpOrDown U ->
                Algorithm.empty

            UpOrDown D ->
                Algorithm.fromTurnList
                    [ Algorithm.Turn Algorithm.X Algorithm.Halfway Algorithm.Clockwise ]

            LeftOrRight R ->
                Algorithm.fromTurnList
                    [ Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.CounterClockwise ]

            LeftOrRight L ->
                Algorithm.fromTurnList
                    [ Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise ]

            FrontOrBack F ->
                Algorithm.fromTurnList
                    [ Algorithm.Turn Algorithm.X Algorithm.OneQuarter Algorithm.Clockwise ]

            FrontOrBack B ->
                Algorithm.fromTurnList
                    [ Algorithm.Turn Algorithm.X Algorithm.OneQuarter Algorithm.CounterClockwise ]


hasStartingOrientation : Algorithm -> Bool
hasStartingOrientation algorithm =
    faces
        |> List.Nonempty.map
            (\face ->
                ( centerColorOnFace
                    face
                    (render <| applyAlgorithm algorithm solved)
                , centerColorOnFace face (render solved)
                )
            )
        |> List.Nonempty.all (\( colorA, colorB ) -> colorA == colorB)
