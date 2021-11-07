module Cube.Advanced exposing
    ( Cube
    , solved
    , applyAlgorithm
    , DisplayAngle, ufrDisplayAngle, ublDisplayAngle, view
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

@docs DisplayAngle, ufrDisplayAngle, ublDisplayAngle, view


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
view =
    viewHelper
        { turnCurrentlyAnimating = Nothing
        }


viewHelper :
    { turnCurrentlyAnimating : Maybe Algorithm.Turn
    }
    -> List (Attribute msg)
    ->
        { a
            | pixelSize : Int
            , displayAngle : DisplayAngle
            , annotateFaces : Bool
        }
    -> Cube
    -> Html msg
viewHelper { turnCurrentlyAnimating } attributes { pixelSize, displayAngle, annotateFaces } cube =
    let
        rotation =
            case displayAngle of
                UFRDisplayAngle ->
                    []

                UBLDisplayAngle ->
                    [ YRotateDegrees 180 ]
    in
    getCubeHtml attributes
        { rotation = rotation
        , turnCurrentlyAnimating = turnCurrentlyAnimating
        , pixelSize = pixelSize
        , annotateFaces = annotateFaces
        }
        cube



-- PARAMETERS


type alias CubeTheme =
    { up : CssColor
    , down : CssColor
    , right : CssColor
    , left : CssColor
    , front : CssColor
    , back : CssColor
    , plastic : CssColor
    }


type alias CssColor =
    String


defaultTheme : CubeTheme
defaultTheme =
    { up = "white"
    , down = "yellow"
    , right = "red"
    , left = "orange"
    , front = "green"
    , back = "blue"
    , plastic = "black"
    }


containerSideLength : Int -> Int
containerSideLength size =
    size * 1


wholeCubeSideLength : Int -> Int
wholeCubeSideLength size =
    containerSideLength size
        |> toFloat
        |> (\x -> x / 1.4)
        |> round


cubieSideLength : Int -> Int
cubieSideLength size =
    wholeCubeSideLength size
        |> toFloat
        |> (\x -> x / 3)
        |> round


cubieBorderWidth : Int -> Int
cubieBorderWidth size =
    cubieSideLength size
        |> toFloat
        |> (\x -> x / 10)
        |> round



-- HTML


type alias Size =
    Int


type alias Vertex =
    { color : Vec3
    , position : Vec3
    }


type alias Uniforms =
    { perspective : Mat4
    , rotation : Mat4
    }


getCubeHtml :
    List (Attribute msg)
    ->
        { rotation : Rotation
        , turnCurrentlyAnimating : Maybe Algorithm.Turn
        , pixelSize : Size
        , annotateFaces : Bool
        }
    -> Cube
    -> Html msg
getCubeHtml attributes { rotation, turnCurrentlyAnimating, annotateFaces, pixelSize } cube =
    WebGL.toHtml
        [ width (pixelSize * 2)
        , height (pixelSize * 2)
        , style "width" (String.fromInt pixelSize ++ "px")
        , style "height" (String.fromInt pixelSize ++ "px")
        , style "display" "block"
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            meshF
            { perspective =
                perspective
            , rotation =
                rotationToWebgl rotation
            }
        , WebGL.entity
            vertexShader
            fragmentShader
            meshF
            { perspective =
                perspective
            , rotation =
                rotationToWebgl rotation
            }
        ]


rotationToWebgl : Rotation -> Mat4
rotationToWebgl rotation =
    List.foldl
        (\singleTransform currentMatrix ->
            case singleTransform of
                XRotateDegrees deg ->
                    Mat4.rotate (degrees deg) Vec3.i currentMatrix

                YRotateDegrees deg ->
                    Mat4.rotate (degrees deg) Vec3.j currentMatrix

                ZRotateDegrees deg ->
                    Mat4.rotate (degrees deg) Vec3.k currentMatrix
        )
        Mat4.identity
        rotation


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


type alias CubiesPositions =
    List ( Float, Float, Float )


cubeMesh : WebGL.Mesh Vertex
cubeMesh =
    List.map2
        (\col pos -> cubieMesh col pos)
        cubiesColours
        initialCubiesPositions
        |> List.concat
        |> WebGL.triangles


cubieMesh : List Vec3 -> ( Float, Float, Float ) -> List ( Vertex, Vertex, Vertex )
cubieMesh colours center =
    let
        width =
            0.97

        rft =
            buildCubieCorner "rft" { center = center, width = width }

        lft =
            buildCubieCorner "lft" { center = center, width = width }

        lbt =
            buildCubieCorner "lbt" { center = center, width = width }

        rbt =
            buildCubieCorner "rbt" { center = center, width = width }

        rbb =
            buildCubieCorner "rbb" { center = center, width = width }

        rfb =
            buildCubieCorner "rfb" { center = center, width = width }

        lfb =
            buildCubieCorner "lfb" { center = center, width = width }

        lbb =
            buildCubieCorner "lbb" { center = center, width = width }
    in
    (case colours of
        [ top, bottom, front, back, left, right ] ->
            [ cubieFace top rft rfb rbb rbt
            , cubieFace bottom rft rfb lfb lft
            , cubieFace front rft lft lbt rbt
            , cubieFace back rfb lfb lbb rbb
            , cubieFace left lft lfb lbb lbt
            , cubieFace right rbt rbb lbb lbt
            ]

        _ ->
            [ cubieFace green rft rfb rbb rbt
            , cubieFace blue rft rfb lfb lft
            , cubieFace yellow rft lft lbt rbt
            , cubieFace red rfb lfb lbb rbb
            , cubieFace white lft lfb lbb lbt
            , cubieFace orange rbt rbb lbb lbt
            ]
    )
        |> List.concat


{-| The string must be three characters long with
the first character being r or l for right or left,
the second one f or b for front or back and the last
one t or b for top or bottom
-}
buildCubieCorner : String -> { center : ( Float, Float, Float ), width : Float } -> Vec3
buildCubieCorner cornerType { center, width } =
    case (String.toLower >> String.toList) cornerType of
        [ rOrL, fOrB, tOrB ] ->
            let
                ( centerX, centerY, centerZ ) =
                    center

                x =
                    case rOrL of
                        'r' ->
                            centerX + width / 2

                        'l' ->
                            centerX - width / 2

                        _ ->
                            0

                y =
                    case fOrB of
                        'f' ->
                            centerY + width / 2

                        'b' ->
                            centerY - width / 2

                        _ ->
                            0

                z =
                    case tOrB of
                        't' ->
                            centerZ + width / 2

                        'b' ->
                            centerZ - width / 2

                        _ ->
                            0
            in
            Vec3.vec3 x y z

        _ ->
            Vec3.vec3 0 0 0


cubieFace : Vec3 -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List ( Vertex, Vertex, Vertex )
cubieFace color a b c d =
    let
        vertex position =
            Vertex (Vec3.scale (1 / 255) color) position
    in
    [ ( vertex a, vertex b, vertex c )
    , ( vertex c, vertex d, vertex a )
    ]


vertexShader : WebGL.Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 rotation;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * rotation * vec4(position, 1.0);
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


white : Vec3
white =
    Vec3.vec3 200 200 200


red : Vec3
red =
    Vec3.vec3 255 0 0


blue : Vec3
blue =
    Vec3.vec3 0 0 255


orange : Vec3
orange =
    Vec3.vec3 245 121 0


green : Vec3
green =
    Vec3.vec3 0 255 0


yellow : Vec3
yellow =
    Vec3.vec3 237 212 0


black : Vec3
black =
    Vec3.vec3 0 0 0


cubiesColours : List (List Vec3)
cubiesColours =
    [ [ black, black, black, blue, red, white ]
    , [ black, black, black, black, red, white ]
    , [ black, black, green, black, red, white ]
    , [ black, black, black, blue, red, black ]
    , [ black, black, black, black, red, black ]
    , [ black, black, green, black, red, black ]
    , [ black, yellow, black, blue, red, black ]
    , [ black, yellow, black, black, red, black ]
    , [ black, yellow, green, black, red, black ]
    , [ black, black, black, blue, black, white ]
    , [ black, black, black, black, black, white ]
    , [ black, black, green, black, black, white ]
    , [ black, black, black, blue, black, black ]
    , [ black, black, black, black, black, black ]
    , [ black, black, green, black, black, black ]
    , [ black, yellow, black, blue, black, black ]
    , [ black, yellow, black, black, black, black ]
    , [ black, yellow, green, black, black, black ]
    , [ orange, black, black, blue, black, white ]
    , [ orange, black, black, black, black, white ]
    , [ orange, black, green, black, black, white ]
    , [ orange, black, black, blue, black, black ]
    , [ orange, black, black, black, black, black ]
    , [ orange, black, green, black, black, black ]
    , [ orange, yellow, black, blue, black, black ]
    , [ orange, yellow, black, black, black, black ]
    , [ orange, yellow, green, black, black, black ]
    ]



-- The position in space of each cubie


initialCubiesPositions : CubiesPositions
initialCubiesPositions =
    [ ( -1, -1, -1 ) -- 0
    , ( -1, -1, 0 ) -- 1
    , ( -1, -1, 1 ) -- 2
    , ( -1, 0, -1 ) -- 3
    , ( -1, 0, 0 ) -- 4
    , ( -1, 0, 1 ) -- 5
    , ( -1, 1, -1 ) -- 6
    , ( -1, 1, 0 ) -- 7
    , ( -1, 1, 1 ) -- 8
    , ( 0, -1, -1 ) -- 9
    , ( 0, -1, 0 ) -- 10
    , ( 0, -1, 1 ) -- 11
    , ( 0, 0, -1 ) -- 12
    , ( 0, 0, 0 ) -- 13
    , ( 0, 0, 1 ) -- 14
    , ( 0, 1, -1 ) -- 15
    , ( 0, 1, 0 ) -- 16
    , ( 0, 1, 1 ) -- 17
    , ( 1, -1, -1 ) -- 18
    , ( 1, -1, 0 ) -- 19
    , ( 1, -1, 1 ) -- 20
    , ( 1, 0, -1 ) -- 21
    , ( 1, 0, 0 ) -- 22
    , ( 1, 0, 1 ) -- 23
    , ( 1, 1, -1 ) -- 24
    , ( 1, 1, 0 ) -- 25
    , ( 1, 1, 1 ) -- 26
    ]



-- let
--     rendering =
--         render cube
-- in
-- div
--     ([ style "width" (px <| containerSideLength pixelSize)
--      , style "height" (px <| containerSideLength pixelSize)
--      , style "display" "flex"
--      , style "justify-content" "center"
--      , style "align-items" "center"
--      , style "perspective" "0"
--      ]
--         ++ attributes
--     )
--     [ div
--         [ style "width" (px <| wholeCubeSideLength pixelSize)
--         , style "height" (px <| wholeCubeSideLength pixelSize)
--         , style "position" "relative"
--         , style "transform-style" "preserve-3d"
--         , cssTransformCube rotation (wholeCubeSideLength pixelSize)
--         ]
--       <|
--         List.map
--             (\( cubieRendering, coordinates, extraCubieAnnotations ) ->
--                 displayCubie
--                     { generateExtraStyles = generateExtraCubieStyles
--                     , theme = defaultTheme
--                     , size = pixelSize
--                     , coordinates = coordinates
--                     , turnCurrentlyAnimating = turnCurrentlyAnimating
--                     , extraCubieAnnotations = extraCubieAnnotations
--                     }
--                     cubieRendering
--             )
--             (getRenderedCorners { annotateFaces = annotateFaces } rendering
--                 |> List.Nonempty.append
--                     (getRenderedEdges { annotateFaces = annotateFaces } rendering)
--                 |> List.Nonempty.append
--                     (getRenderedCenters { annotateFaces = annotateFaces } rendering)
--                 |> List.Nonempty.toList
--             )
--     ]
-- LOGIC AND MAPPINGS


px : Int -> String
px pixels =
    String.fromInt pixels ++ "px"


getFaceAnnotation : ExtraCubieAnnotations msg -> Face -> Maybe (Size -> Html msg)
getFaceAnnotation annotations face =
    case face of
        UpOrDown U ->
            annotations.u

        UpOrDown D ->
            annotations.d

        FrontOrBack F ->
            annotations.f

        FrontOrBack B ->
            annotations.b

        LeftOrRight L ->
            annotations.l

        LeftOrRight R ->
            annotations.r


getFaceRotation : Face -> SingleRotation
getFaceRotation face =
    case face of
        UpOrDown U ->
            XRotateDegrees 90

        UpOrDown D ->
            XRotateDegrees -90

        FrontOrBack F ->
            XRotateDegrees 0

        FrontOrBack B ->
            YRotateDegrees 180

        LeftOrRight L ->
            YRotateDegrees -90

        LeftOrRight R ->
            YRotateDegrees 90


getFaceColor : Face -> CubieRendering -> Color
getFaceColor face rendering =
    case face of
        UpOrDown U ->
            rendering.u

        UpOrDown D ->
            rendering.d

        FrontOrBack F ->
            rendering.f

        FrontOrBack B ->
            rendering.b

        LeftOrRight L ->
            rendering.l

        LeftOrRight R ->
            rendering.r


getColorString : CubeTheme -> Color -> String
getColorString theme color =
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


getRenderedCorners : { annotateFaces : Bool } -> Rendering -> List.Nonempty.Nonempty ( CubieRendering, Coordinates, ExtraCubieAnnotations msg )
getRenderedCorners arguments rendering =
    List.Nonempty.map (getRenderedCorner arguments rendering) cornerLocations


getRenderedCorner : { annotateFaces : Bool } -> Rendering -> CornerLocation -> ( CubieRendering, Coordinates, ExtraCubieAnnotations msg )
getRenderedCorner _ rendering location =
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
    ( cornerRendering, getCornerCoordinates location, noAnnotations )


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


type alias ExtraCubieAnnotations msg =
    { u : Maybe (Size -> Html msg)
    , d : Maybe (Size -> Html msg)
    , f : Maybe (Size -> Html msg)
    , b : Maybe (Size -> Html msg)
    , l : Maybe (Size -> Html msg)
    , r : Maybe (Size -> Html msg)
    }


noAnnotations : ExtraCubieAnnotations msg
noAnnotations =
    { u = Nothing
    , d = Nothing
    , f = Nothing
    , b = Nothing
    , l = Nothing
    , r = Nothing
    }


getRenderedEdges : { annotateFaces : Bool } -> Rendering -> List.Nonempty.Nonempty ( CubieRendering, Coordinates, ExtraCubieAnnotations msg )
getRenderedEdges arguments rendering =
    List.Nonempty.map (getRenderedEdge arguments rendering) edgeLocations


getRenderedEdge : { annotateFaces : Bool } -> Rendering -> EdgeLocation -> ( CubieRendering, Coordinates, ExtraCubieAnnotations msg )
getRenderedEdge _ rendering location =
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
    ( edgeRendering, getEdgeCoordinates location, noAnnotations )


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


getRenderedCenters : { annotateFaces : Bool } -> Rendering -> List.Nonempty.Nonempty ( CubieRendering, Coordinates, ExtraCubieAnnotations msg )
getRenderedCenters arguments rendering =
    List.Nonempty.map (getRenderedCenter arguments rendering) centerLocations


getRenderedCenter : { annotateFaces : Bool } -> Rendering -> CenterLocation -> ( CubieRendering, Coordinates, ExtraCubieAnnotations msg )
getRenderedCenter { annotateFaces } rendering location =
    let
        ( centerRendering, textAnnotations ) =
            case location of
                CenterLocation (UpOrDown U) ->
                    ( rendering.u, { noAnnotations | u = Just (px >> svgU) } )

                CenterLocation (UpOrDown D) ->
                    ( rendering.d, { noAnnotations | d = Just (px >> svgD) } )

                CenterLocation (LeftOrRight L) ->
                    ( rendering.l, { noAnnotations | l = Just (px >> svgL) } )

                CenterLocation (LeftOrRight R) ->
                    ( rendering.r, { noAnnotations | r = Just (px >> svgR) } )

                CenterLocation (FrontOrBack F) ->
                    ( rendering.f, { noAnnotations | f = Just (px >> svgF) } )

                CenterLocation (FrontOrBack B) ->
                    ( rendering.b, { noAnnotations | b = Just (px >> svgB) } )
    in
    ( centerRendering
    , getCenterCoordinates location
    , if annotateFaces then
        textAnnotations

      else
        noAnnotations
    )


svgF : String -> Html msg
svgF size =
    svg [ viewBox "0 0 150 225", Svg.Attributes.height size ]
        [ line [ x1 "15", y1 "0", x2 "15", y2 "225", stroke "black", strokeWidth "30" ] []
        , line [ x1 "0", y1 "12.5", x2 "150", y2 "12.5", stroke "black", strokeWidth "25" ] []
        , line [ x1 "0", y1 "112.5", x2 "130", y2 "112.5", stroke "black", strokeWidth "25" ] []
        ]


meshF : WebGL.Mesh Vertex
meshF =
    WebGL.triangles
        (triangleLine { from = Vec2.vec2 0 0, to = Vec2.vec2 1 1, zCoordinate = 0, width = 0.02, color = black })


triangleLine : { from : Vec2, to : Vec2, zCoordinate : Float, width : Float, color : Vec3 } -> List ( Vertex, Vertex, Vertex )
triangleLine { from, to, width, color, zCoordinate } =
    let
        diff =
            Vec2.direction from to

        normal =
            Vec2.vec2 -(Vec2.getY diff) (Vec2.getX diff)

        halfWidthLengthNormal =
            Vec2.scale (width / 2) normal

        dotProduct1 =
            Debug.log "1. should be 0" Vec2.dot normal diff

        dotProduct2 =
            Debug.log "2. should be 0" Vec2.dot halfWidthLengthNormal diff

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
    { position = position, color = color }


mapTriple : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTriple fn ( x, y, z ) =
    ( fn x, fn y, fn z )


svgL : String -> Html msg
svgL size =
    svg [ viewBox "-15 0 150 225", Svg.Attributes.height size ]
        [ line [ x1 "0", y1 "0", x2 "0", y2 "225", stroke "black", strokeWidth "30" ] []
        , line [ x1 "0", y1 "212.5", x2 "150", y2 "212.5", stroke "black", strokeWidth "25" ] []
        ]


svgU : String -> Html msg
svgU size =
    svg [ viewBox "-17.5 0 219 300", Svg.Attributes.height size ]
        [ path [ d "M 0,0 l 0,200 a 92.5,82.5 0 0 0 185,0 l 0,-200", fill "transparent", strokeWidth "35", stroke "black" ] []
        ]


svgD : String -> Html msg
svgD size =
    svg [ viewBox "-17.5 0 230 290", Svg.Attributes.height size ]
        [ path [ d "M 0,0 l 0,272.5 l 100,0 a 95,127.5 0 0 0 0,-255 l -100,0", fill "transparent", strokeWidth "35", stroke "black" ] []
        ]


svgR : String -> Html msg
svgR size =
    svg [ viewBox "-17.5 0 255 290", Svg.Attributes.height size ]
        [ path [ d "M 0,290 l 0,-272.5 l 100,0 a 95,63.75 0 0 1 0,127.5 l -100,0 l 120,0 l 200,300", fill "transparent", strokeWidth "35", stroke "black" ] []
        ]


svgB : String -> Html msg
svgB size =
    svg [ viewBox "-17.5 0 230 290", Svg.Attributes.height size ]
        [ path [ d "M 0,290 l 0,-272.5 l 100,0 a 95,63.75 0 0 1 0,127.5 l -100,0 m 100,0 a 95,63.75 0 0 1 0,127.5 l -100,0", fill "transparent", strokeWidth "35", stroke "black" ] []
        ]


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



-- ANIMATION IMPLEMENTATIONS


{-| See [Cube.AnimationState](Cube#AnimationState)
-}
type AnimationState
    = AnimationState
        { toApply : List Algorithm.Turn
        , alreadyApplied : Algorithm
        , inBetweenTurns : Bool
        , paused : Bool
        }


{-| See [Cube.animateAlgorithm](Cube#animateAlgorithm)
-}
animateAlgorithm : Algorithm -> AnimationState
animateAlgorithm algorithm =
    AnimationState
        { toApply = Algorithm.toTurnList algorithm
        , alreadyApplied = Algorithm.empty
        , inBetweenTurns = False
        , paused = False
        }


{-| See [Cube.noAnimation](Cube#noAnimation)
-}
noAnimation : AnimationState
noAnimation =
    AnimationState
        { toApply = []
        , alreadyApplied = Algorithm.empty
        , inBetweenTurns = True
        , paused = False
        }


{-| See [Cube.pauseAnimation](Cube#pauseAnimation)
-}
pauseAnimation : AnimationState -> AnimationState
pauseAnimation (AnimationState animationState) =
    AnimationState { animationState | paused = True }


{-| See [Cube.unpauseAnimation](Cube#unpauseAnimation)
-}
unpauseAnimation : AnimationState -> AnimationState
unpauseAnimation (AnimationState animationState) =
    AnimationState { animationState | paused = False, inBetweenTurns = False }


{-| See [Cube.currentTurnAnimating](Cube#currentTurnAnimating)
-}
currentTurnAnimating : AnimationState -> Maybe Algorithm.Turn
currentTurnAnimating (AnimationState { toApply, inBetweenTurns, paused }) =
    if inBetweenTurns || paused then
        Nothing

    else
        List.head toApply


{-| See [Cube.viewAnimatable](Cube#viewAnimatable)
-}
viewAnimatable :
    List (Attribute msg)
    ->
        { animationState : AnimationState
        , toMsg : AnimationMsg -> msg
        , pixelSize : Int
        , displayAngle : DisplayAngle
        , annotateFaces : Bool
        }
    -> Cube
    -> Html msg
viewAnimatable attributes ({ animationState, toMsg } as arguments) cube =
    let
        (AnimationState animationStateInternal) =
            animationState

        cubeIncludingAnimatedTurns =
            applyAlgorithm animationStateInternal.alreadyApplied cube
    in
    viewHelper
        { turnCurrentlyAnimating = currentTurnAnimating animationState
        }
        (List.map (Html.Attributes.map UserMsg) attributes)
        arguments
        cubeIncludingAnimatedTurns
        |> Html.map (unwrapAnimationOrUserMsg toMsg)


wrappedAnimationStyle : Size -> Coordinates -> Maybe Algorithm.Turn -> List (Attribute (AnimationOrUserMsg msg))
wrappedAnimationStyle a b c =
    List.map (Html.Attributes.map AnimationMsg) (animationStyle a b c)


type AnimationOrUserMsg msg
    = AnimationMsg AnimationMsg
    | UserMsg msg


unwrapAnimationOrUserMsg : (AnimationMsg -> msg) -> AnimationOrUserMsg msg -> msg
unwrapAnimationOrUserMsg toMsg wrappedMsg =
    case wrappedMsg of
        AnimationMsg animationMsg ->
            toMsg animationMsg

        UserMsg msg ->
            msg


{-| See [Cube.AnimationMsg](Cube#AnimationMsg)
-}
type AnimationMsg
    = TurnFinished
    | StartNextTurn


{-| See [Cube.handleAnimationMsg](Cube#handleAnimationMsg)
-}
handleAnimationMsg :
    { toMsg : AnimationMsg -> msg, animationDoneMsg : msg }
    -> AnimationState
    -> AnimationMsg
    -> ( AnimationState, Cmd msg )
handleAnimationMsg { toMsg, animationDoneMsg } (AnimationState animationState) msg =
    case msg of
        TurnFinished ->
            if animationState.inBetweenTurns then
                ( AnimationState animationState, Cmd.none )

            else
                case animationState.toApply of
                    [] ->
                        ( AnimationState { animationState | inBetweenTurns = True }
                        , Task.perform (always animationDoneMsg) (Task.succeed ())
                        )

                    x :: xs ->
                        ( AnimationState
                            { animationState
                                | toApply = xs
                                , alreadyApplied =
                                    Algorithm.append
                                        animationState.alreadyApplied
                                        (Algorithm.fromTurnList [ x ])
                                , inBetweenTurns = True
                            }
                        , if xs /= [] then
                            Task.perform (always (toMsg StartNextTurn)) (Process.sleep 0)

                          else
                            Task.perform (always animationDoneMsg) (Task.succeed ())
                        )

        StartNextTurn ->
            if animationState.paused then
                ( AnimationState animationState, Cmd.none )

            else
                ( AnimationState { animationState | inBetweenTurns = False }, Cmd.none )


animationStyle : Size -> Coordinates -> Maybe Algorithm.Turn -> List (Attribute AnimationMsg)
animationStyle size coordinates animationTurn =
    animationTurn
        |> Maybe.andThen
            (\turn ->
                if isCubieTurning turn coordinates then
                    Just turn

                else
                    Nothing
            )
        |> Maybe.map
            (\turn ->
                [ Html.Events.on "transitionend" (Json.Decode.succeed TurnFinished)
                , style "transition"
                    ("transform "
                        ++ String.fromInt (getTurnMilliseconds turn)
                        ++ "ms"
                    )
                , style "transform"
                    (getTurnTransformation turn)
                , style "transform-style" "preserve-3d"
                , style "transform-origin" <|
                    String.fromInt (wholeCubeSideLength size // 2)
                        ++ "px "
                        ++ String.fromInt (wholeCubeSideLength size // 2)
                        ++ "px "
                        ++ String.fromInt (wholeCubeSideLength size // 2 * -1)
                        ++ "px"
                ]
            )
        |> Maybe.withDefault
            [ style "transform-style" "preserve-3d"
            ]


isCubieTurning : Algorithm.Turn -> Coordinates -> Bool
isCubieTurning (Algorithm.Turn turnable _ _) { fromFront, fromLeft, fromTop } =
    case turnable of
        Algorithm.U ->
            fromTop == 0

        Algorithm.E ->
            fromTop == 1

        Algorithm.D ->
            fromTop == 2

        Algorithm.Y ->
            True

        Algorithm.Uw ->
            fromTop /= 2

        Algorithm.Dw ->
            fromTop /= 0

        Algorithm.L ->
            fromLeft == 0

        Algorithm.M ->
            fromLeft == 1

        Algorithm.R ->
            fromLeft == 2

        Algorithm.Z ->
            True

        Algorithm.Rw ->
            fromLeft /= 0

        Algorithm.Lw ->
            fromLeft /= 2

        Algorithm.F ->
            fromFront == 0

        Algorithm.S ->
            fromFront == 1

        Algorithm.B ->
            fromFront == 2

        Algorithm.Fw ->
            fromFront /= 2

        Algorithm.Bw ->
            fromFront /= 0

        Algorithm.X ->
            True


getTurnMilliseconds : Algorithm.Turn -> Int
getTurnMilliseconds (Algorithm.Turn _ turnLength _) =
    case turnLength of
        Algorithm.OneQuarter ->
            750

        Algorithm.Halfway ->
            1250

        Algorithm.ThreeQuarters ->
            1750


getTurnTransformation : Algorithm.Turn -> String
getTurnTransformation (Algorithm.Turn turnable turnLength turnDirection) =
    let
        degrees =
            case turnLength of
                Algorithm.OneQuarter ->
                    "90deg"

                Algorithm.Halfway ->
                    "180deg"

                Algorithm.ThreeQuarters ->
                    "270deg"

        axis =
            case turnable of
                Algorithm.U ->
                    "Y"

                Algorithm.E ->
                    "Y"

                Algorithm.D ->
                    "Y"

                Algorithm.Y ->
                    "Y"

                Algorithm.Uw ->
                    "Y"

                Algorithm.Dw ->
                    "Y"

                Algorithm.L ->
                    "X"

                Algorithm.M ->
                    "X"

                Algorithm.R ->
                    "X"

                Algorithm.Z ->
                    "Z"

                Algorithm.Rw ->
                    "X"

                Algorithm.Lw ->
                    "X"

                Algorithm.F ->
                    "Z"

                Algorithm.S ->
                    "Z"

                Algorithm.B ->
                    "Z"

                Algorithm.Fw ->
                    "Z"

                Algorithm.Bw ->
                    "Z"

                Algorithm.X ->
                    "X"

        shouldHaveMinusSign =
            case turnable of
                Algorithm.U ->
                    turnDirection == Algorithm.Clockwise

                Algorithm.E ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.D ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.Y ->
                    turnDirection == Algorithm.Clockwise

                Algorithm.Uw ->
                    turnDirection == Algorithm.Clockwise

                Algorithm.Dw ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.L ->
                    turnDirection == Algorithm.Clockwise

                Algorithm.M ->
                    turnDirection == Algorithm.Clockwise

                Algorithm.R ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.Z ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.Rw ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.Lw ->
                    turnDirection == Algorithm.Clockwise

                Algorithm.F ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.S ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.B ->
                    turnDirection == Algorithm.Clockwise

                Algorithm.Fw ->
                    turnDirection == Algorithm.CounterClockwise

                Algorithm.Bw ->
                    turnDirection == Algorithm.Clockwise

                Algorithm.X ->
                    turnDirection == Algorithm.CounterClockwise
    in
    "rotate"
        ++ axis
        ++ "("
        ++ (if shouldHaveMinusSign then
                "-"

            else
                ""
           )
        ++ degrees
        ++ ")"
