module Cube.Advanced exposing
    ( Cube
    , solved
    , applyAlgorithm
    , CubeTheme, defaultTheme, DisplayAngle, ufrDisplayAngle, ublDisplayAngle, dblDisplayAngle, view, debugViewAllowingVisualTesting
    , Rendering, CubieRendering, CubeColor(..), render
    , Face(..), UOrD(..), LOrR(..), FOrB(..), uFace, dFace, rFace, lFace, fFace, bFace, faceToColor, colorToFaceItBelongsTo, setColor, faces, CornerLocation, getCorner, setCorner, cornerLocations, EdgeLocation(..), getEdge, setEdge, edgeLocations, CenterLocation, getCenter, setCenter, centerLocations
    , algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation, makeAlgorithmMaintainOrientation
    , addAUFsToAlgorithm, detectAUFs
    , canBeSolvedBySingleUTurn
    )

{-|


# Definition

@docs Cube


# Constructors

@docs solved


# Modifiers

@docs applyAlgorithm


# Displayers

@docs CubeTheme, defaultTheme, DisplayAngle, ufrDisplayAngle, ublDisplayAngle, dblDisplayAngle, view, debugViewAllowingVisualTesting


# Rendering

@docs Rendering, CubieRendering, CubeColor, render


## Rendering Helpers

@docs Face, UOrD, LOrR, FOrB, uFace, dFace, rFace, lFace, fFace, bFace, faceToColor, colorToFaceItBelongsTo, setColor, faces, CornerLocation, getCorner, setCorner, cornerLocations, EdgeLocation, getEdge, setEdge, edgeLocations, CenterLocation, getCenter, setCenter, centerLocations


# Algorithm Helpers

@docs algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation, makeAlgorithmMaintainOrientation


# AUF Helpers

@docs addAUFsToAlgorithm, detectAUFs


# General Helpers

@docs canBeSolvedBySingleUTurn

-}

import AUF exposing (AUF)
import Algorithm exposing (Algorithm)
import Color exposing (Color)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy
import List.Extra
import List.Nonempty
import Math.Matrix4 as Mat4 exposing (Mat4)
import Math.Vector2 as Vec2 exposing (Vec2)
import Math.Vector3 as Vec3 exposing (Vec3)
import Utils.Enumerator
import Utils.MappedPermutation as MappedPermutation exposing (MappedPermutation)
import WebGL
import WebGL.Extra



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
        -- Single Face Turns
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

        -- Slice Turns
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

        -- Wide Turns
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

        -- Cube Rotations
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
if you for example want to display the cube differently than this package
does, or you need to introspect specific state of the cube for your application.

It is expected that most use cases should be able to fulfill their goals without the
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
    { u : CubeColor
    , d : CubeColor
    , f : CubeColor
    , b : CubeColor
    , l : CubeColor
    , r : CubeColor
    }


{-| The color of a face of a cubie. The name of the color corresponds to
the face of the cube that it belongs to in the initial orientation. So in
standard scrambling orientation of the 3x3 cube UpColor is white and
RightColor is red. PlasticColor means it is a part of the cubie not facing
outwards
-}
type CubeColor
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


getCornerColorOnReferenceFace : OrientedCorner -> CubeColor
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


getCornerColorOnClockwiseFace : OrientedCorner -> CubeColor
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


getCornerColorOnCounterClockwiseFace : OrientedCorner -> CubeColor
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


getEdgeColorOnReferenceFace : OrientedEdge -> CubeColor
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


getEdgeColorOnOtherFace : OrientedEdge -> CubeColor
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


getCentersColor : Center -> CubeColor
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
setColor : Face -> CubeColor -> CubieRendering -> CubieRendering
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
faceToColor : Face -> CubeColor
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


{-| Get the color corresponding to the given face
-}
colorToFaceItBelongsTo : CubeColor -> Maybe Face
colorToFaceItBelongsTo color =
    case color of
        PlasticColor ->
            Nothing

        UpColor ->
            Just <| UpOrDown U

        DownColor ->
            Just <| UpOrDown D

        FrontColor ->
            Just <| FrontOrBack F

        BackColor ->
            Just <| FrontOrBack B

        LeftColor ->
            Just <| LeftOrRight L

        RightColor ->
            Just <| LeftOrRight R



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



-- ALGORITHM RELEVANT APIS


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
        cubeWithAlgorithmAppliedFromSolved =
            applyAlgorithm algorithm solved

        faceToMoveToU =
            findFaceWithCenterColor
                UpColor
                cubeWithAlgorithmAppliedFromSolved

        additionThatFixesU =
            rotateSoFaceIsOnU faceToMoveToU

        cubeWithUFixed =
            applyAlgorithm additionThatFixesU cubeWithAlgorithmAppliedFromSolved

        algorithmWithUFixed =
            Algorithm.append algorithm additionThatFixesU

        faceToMoveToF =
            findFaceWithCenterColor
                FrontColor
                cubeWithUFixed

        additionThatFixesF =
            rotateSoFaceIsOnFWhileMaintainingU faceToMoveToF
    in
    Algorithm.append algorithmWithUFixed additionThatFixesF


findFaceWithCenterColor : CubeColor -> Cube -> Face
findFaceWithCenterColor color cube =
    faces
        |> List.Nonempty.toList
        |> List.map (\face -> ( centerColorOnFace face cube, face ))
        |> List.Extra.find (Tuple.first >> (==) color)
        |> Maybe.map Tuple.second
        -- We trust the tests here by using the default as this case should
        -- never happen but in case it does good unit tests hopefully catch it
        |> Maybe.withDefault (UpOrDown U)


centerColorOnFace : Face -> Cube -> CubeColor
centerColorOnFace face (Cube _ _ centerPositions) =
    getCentersColor <|
        case face of
            UpOrDown U ->
                centerPositions.u

            UpOrDown D ->
                centerPositions.d

            LeftOrRight R ->
                centerPositions.r

            LeftOrRight L ->
                centerPositions.l

            FrontOrBack F ->
                centerPositions.f

            FrontOrBack B ->
                centerPositions.b


rotateSoFaceIsOnU : Face -> Algorithm
rotateSoFaceIsOnU face =
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


rotateSoFaceIsOnFWhileMaintainingU : Face -> Algorithm
rotateSoFaceIsOnFWhileMaintainingU face =
    case face of
        UpOrDown U ->
            Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.X Algorithm.OneQuarter Algorithm.CounterClockwise ]

        UpOrDown D ->
            Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.X Algorithm.OneQuarter Algorithm.Clockwise ]

        LeftOrRight R ->
            Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.Y Algorithm.OneQuarter Algorithm.Clockwise ]

        LeftOrRight L ->
            Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.Y Algorithm.OneQuarter Algorithm.CounterClockwise ]

        FrontOrBack F ->
            Algorithm.empty

        FrontOrBack B ->
            Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.Y Algorithm.Halfway Algorithm.Clockwise ]


{-| Checks whether a cube is only a single U turn away from being solved or not.
This is used internally, which is why it sits here under Advanced as it's a pretty
particular use case, but can be useful for some AUF logic, hopefully most if not
all of your needs should be satisfied by the AUF module though

    import Algorithm

    Algorithm.fromString "U"
        |> Result.map (\alg -> canBeSolvedBySingleUTurn (applyAlgorithm alg solved))
    --> Ok True

    Algorithm.fromString "F"
        |> Result.map (\alg -> canBeSolvedBySingleUTurn (applyAlgorithm alg solved))
    --> Ok False

-}
canBeSolvedBySingleUTurn : Cube -> Bool
canBeSolvedBySingleUTurn cube =
    cube
        |> applyAlgorithm (findOnlyCandidateUTurnThatWouldFixCube cube)
        |> (==) solved


findOnlyCandidateUTurnThatWouldFixCube : Cube -> Algorithm
findOnlyCandidateUTurnThatWouldFixCube (Cube _ edgePositions _) =
    -- This relies on the fact that we say the reference face of an edge
    -- is the one that is currently in the U/D layer. Tests should also catch if this
    -- turns out to change
    case getEdgeColorOnOtherFace edgePositions.uf of
        -- If the FU sticker is up or down the edge is not oriented
        -- and this can't be fixed with a single U turn so we return
        -- anything that won't fix it such as the empty algorithm
        UpColor ->
            Algorithm.empty

        DownColor ->
            Algorithm.empty

        -- There would be a serious issue if the FU sticker was plastic colored
        -- so we can just as well pick any algorithm, but there should be no
        -- way to solve it with legal moves
        PlasticColor ->
            Algorithm.empty

        -- All these stickers have exactly one U turn that would fix them
        -- so we don't need to consider any other candidates as any other
        -- single move would not fix this sticker which must be fixed in
        -- order to fix the cube
        FrontColor ->
            Algorithm.empty

        RightColor ->
            Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
                ]

        BackColor ->
            Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
                ]

        LeftColor ->
            Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
                ]


{-| See [Cube.addAUFsToAlgorithm](Cube#addAUFsToAlgorithm)
-}
addAUFsToAlgorithm : ( AUF, AUF ) -> Algorithm -> Algorithm
addAUFsToAlgorithm ( preAUF, postAUF ) algorithm =
    let
        -- Getting the center cubies of the state of a cube where the algorithm
        -- was applied to a solved cube so we can determine the final orientation
        (Cube _ _ { u, f, b, l, r, d }) =
            applyAlgorithm algorithm solved

        postAUFTurnable =
            [ ( u, Algorithm.U )
            , ( d, Algorithm.D )
            , ( f, Algorithm.F )
            , ( b, Algorithm.B )
            , ( l, Algorithm.L )
            , ( r, Algorithm.R )
            ]
                |> List.Extra.find (Tuple.first >> (==) UCenter)
                |> Maybe.map Tuple.second
                -- This should never occur but unit tests should catch if it does
                |> Maybe.withDefault Algorithm.U
    in
    Algorithm.append (AUF.toAlgorithm preAUF) <|
        Algorithm.append algorithm <|
            AUF.toAlgorithmWithCustomTurnable postAUFTurnable postAUF


{-| See [Cube.detectAUFs](Cube#detectAUFs)
-}
detectAUFs : { toDetectFor : Algorithm, toMatchTo : Algorithm } -> Maybe ( AUF, AUF )
detectAUFs { toDetectFor, toMatchTo } =
    let
        -- We reimplement some algorithm equivalency algorithms here to save on some recomputation
        -- and optimize the runtime
        cubeAlgorithmShouldSolve =
            applyAlgorithm
                (Algorithm.inverse <|
                    makeAlgorithmMaintainOrientation toMatchTo
                )
                solved

        toDetectForWithMaintainedOrientation =
            makeAlgorithmMaintainOrientation toDetectFor
    in
    AUF.all
        |> List.Nonempty.toList
        |> List.Extra.findMap
            (\preAUF ->
                let
                    cubeOnlyMissingLastAUF =
                        cubeAlgorithmShouldSolve
                            |> applyAlgorithm (AUF.toAlgorithm preAUF)
                            |> applyAlgorithm toDetectForWithMaintainedOrientation

                    onlyUCandidate =
                        findOnlyCandidateUTurnThatWouldFixCube cubeOnlyMissingLastAUF

                    postAUF =
                        AUF.fromAlgorithm onlyUCandidate
                            -- We trust unit tests should catch this case as it should never occur
                            |> Maybe.withDefault AUF.None
                in
                if applyAlgorithm onlyUCandidate cubeOnlyMissingLastAUF == solved then
                    Just ( preAUF, postAUF )

                else
                    Nothing
            )



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
        , theme : CubeTheme
        }
    -> Cube
    -> Html msg
view =
    viewHelper { preserveDrawingBuffer = False }


{-| It's exactly the same as [Cube.view](Cube#view), except it allows for programmatic screenshotting
of the application, and the cost of that is lowered performance. This is due to some WebGL internals which
the cube rendering is implemented with
-}
debugViewAllowingVisualTesting :
    List (Attribute msg)
    ->
        { pixelSize : Int
        , displayAngle : DisplayAngle
        , annotateFaces : Bool
        , theme : CubeTheme
        }
    -> Cube
    -> Html msg
debugViewAllowingVisualTesting =
    viewHelper { preserveDrawingBuffer = True }


viewHelper :
    { preserveDrawingBuffer : Bool }
    -> List (Attribute msg)
    ->
        { pixelSize : Int
        , displayAngle : DisplayAngle
        , annotateFaces : Bool
        , theme : CubeTheme
        }
    -> Cube
    -> Html msg
viewHelper { preserveDrawingBuffer } attributes { pixelSize, displayAngle, annotateFaces, theme } cube =
    div attributes <|
        List.singleton <|
            Html.Lazy.lazy6 lazyViewHelper preserveDrawingBuffer pixelSize displayAngle annotateFaces theme cube


lazyViewHelper : Bool -> Int -> DisplayAngle -> Bool -> CubeTheme -> Cube -> Html msg
lazyViewHelper preserveDrawingBuffer pixelSize displayAngle annotateFaces theme cube =
    let
        { mainRotation, annotationAdjustments } =
            getRotations displayAngle
    in
    getCubeHtml
        { rotation = mainRotation
        , pixelSize = pixelSize
        , annotateFaces =
            if annotateFaces then
                Just annotationAdjustments

            else
                Nothing
        , theme = theme
        , preserveDrawingBuffer = preserveDrawingBuffer
        }
        cube


getRotations :
    DisplayAngle
    ->
        { mainRotation : Rotation
        , annotationAdjustments :
            { u : Rotation, d : Rotation, f : Rotation, b : Rotation, l : Rotation, r : Rotation }
        }
getRotations displayAngle =
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



-- PARAMETERS


{-| The color scheme of the cube, and also color of the "plastic"
and the face annotations (U, F, B etc. on the respective sides) text color.

Note that we use the color type from [avh4/elm-color](https://package.elm-lang.org/packages/avh4/elm-color/latest/)

-}
type alias CubeTheme =
    { up : Color
    , down : Color
    , right : Color
    , left : Color
    , front : Color
    , back : Color
    , plastic : Color
    , annotations : Color
    }


{-| For ease of use here is a pretty standard color scheme for a cube
-}
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


white : Color
white =
    Color.rgb255 200 200 200


red : Color
red =
    Color.rgb255 255 0 0


blue : Color
blue =
    Color.rgb255 0 0 255


orange : Color
orange =
    Color.rgb255 245 121 0


green : Color
green =
    Color.rgb255 0 255 0


yellow : Color
yellow =
    Color.rgb255 237 212 0


black : Color
black =
    Color.rgb255 0 0 0



-- HTML


type alias Size =
    Int


getCubeHtml :
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
    , preserveDrawingBuffer : Bool
    }
    -> Cube
    -> Html msg
getCubeHtml { rotation, annotateFaces, pixelSize, theme, preserveDrawingBuffer } cube =
    let
        toHtmlOptions =
            if preserveDrawingBuffer then
                WebGL.preserveDrawingBuffer :: WebGL.Extra.toHtmlDefaultOptions

            else
                WebGL.Extra.toHtmlDefaultOptions
    in
    WebGL.toHtmlWith toHtmlOptions
        -- All these properties including the double size of width and height
        -- are all just the suggestions in the toHtml documentation
        [ width (pixelSize * 2)
        , height (pixelSize * 2)
        , style "width" (String.fromInt pixelSize ++ "px")
        , style "height" (String.fromInt pixelSize ++ "px")
        , style "display" "block"
        ]
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


{-| All this WebGL code was modified from the original at
<https://github.com/maxf/elm-webgl-rubik/tree/28f20972aee898c262461b93dbb6e45b67859b29>
-}
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
    (List.map
        -- Add the shared arguments
        (\params ->
            { innerColor = params.innerColor
            , center =
                params.center
                    |> Vec3.scale totalCubieWidth
                    |> Vec3.add center
            , orthogonalPlaneDirection1 = params.orthogonalPlaneDirection1
            , orthogonalPlaneDirection2 = params.orthogonalPlaneDirection2
            , totalWidthAndHeight = totalCubieWidth
            , borderWidth = borderWidth
            , borderColor = theme.plastic
            }
        )
        >> List.map square
        >> List.concat
    )
        [ { innerColor = colors.up

          -- Note that these coordinates are scaled and added to cubie center in the map above
          , center = Vec3.vec3 0 0.5 0
          , orthogonalPlaneDirection1 = Vec3.i
          , orthogonalPlaneDirection2 = Vec3.k
          }
        , { innerColor = colors.down
          , center = Vec3.vec3 0 -0.5 0
          , orthogonalPlaneDirection1 = Vec3.i
          , orthogonalPlaneDirection2 = Vec3.k
          }
        , { innerColor = colors.front
          , center = Vec3.vec3 0 0 0.5
          , orthogonalPlaneDirection1 = Vec3.i
          , orthogonalPlaneDirection2 = Vec3.j
          }
        , { innerColor = colors.back
          , center = Vec3.vec3 0 0 -0.5
          , orthogonalPlaneDirection1 = Vec3.i
          , orthogonalPlaneDirection2 = Vec3.j
          }
        , { innerColor = colors.left
          , center = Vec3.vec3 -0.5 0 0
          , orthogonalPlaneDirection1 = Vec3.j
          , orthogonalPlaneDirection2 = Vec3.k
          }
        , { innerColor = colors.right
          , center = Vec3.vec3 0.5 0 0
          , orthogonalPlaneDirection1 = Vec3.j
          , orthogonalPlaneDirection2 = Vec3.k
          }
        ]


type alias CubieData =
    { colors :
        { up : Color
        , down : Color
        , front : Color
        , back : Color
        , left : Color
        , right : Color
        }
    , center : Vec3
    }


allCubieData : CubeTheme -> Rendering -> List CubieData
allCubieData theme rendering =
    List.map
        (\( cubieRendering, coordinates ) ->
            { center = coordinatesToCenterVector coordinates, colors = cubieRenderingToRgbColors theme cubieRendering }
        )
        (getRenderedCorners rendering
            |> List.Nonempty.append
                (getRenderedEdges rendering)
            |> List.Nonempty.append
                (getRenderedCenters rendering)
            |> List.Nonempty.toList
        )


cubieRenderingToRgbColors :
    CubeTheme
    -> CubieRendering
    ->
        { up : Color
        , down : Color
        , front : Color
        , back : Color
        , left : Color
        , right : Color
        }
cubieRenderingToRgbColors theme rendering =
    { up = rendering.u |> getRgb255Color theme
    , down = rendering.d |> getRgb255Color theme
    , front = rendering.f |> getRgb255Color theme
    , back = rendering.b |> getRgb255Color theme
    , left = rendering.l |> getRgb255Color theme
    , right = rendering.r |> getRgb255Color theme
    }


coordinatesToCenterVector : Coordinates -> Vec3
coordinatesToCenterVector { fromFront, fromTop, fromLeft } =
    Vec3.vec3 (toFloat fromLeft - 1) (1 - toFloat fromTop) (1 - toFloat fromFront)


rotationToWebgl : Rotation -> Mat4 -> Mat4
rotationToWebgl rotation =
    List.foldl
        (\singleTransform currentTransform ->
            case singleTransform of
                YRotateDegrees deg ->
                    Mat4.rotate (degrees deg) Vec3.j << currentTransform

                ZRotateDegrees deg ->
                    Mat4.rotate (degrees deg) Vec3.k << currentTransform
        )
        identity
        rotation



-- ANNOTATIONS


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
    let
        -- The .51 instead of just .5 is so the annotations display in front of the cube instead of blending into the face itself
        distanceFromCenter =
            1.51
    in
    List.map
        (\( letterFn, adjustment, { centerPosition, rotate } ) ->
            WebGL.entity
                vertexShader
                fragmentShader
                (letterFn
                    { centerPosition = centerPosition
                    , rotate = rotate >> rotationToWebgl adjustment

                    -- The height of the letter. The cubie height is 1.0 for reference
                    , height = 0.6

                    -- Adjusting this means a lot for the smoothness of the annotation curves and also greatly impacts performance
                    -- be careful making it too low as this can really make devices work surprisingly hard
                    , granularity = 0.07
                    , color = theme.annotations
                    }
                )
                { perspective = perspective
                , rotation = rotationToWebgl rotation Mat4.identity
                }
        )
        [ ( meshF, adjustments.f, { centerPosition = Vec3.vec3 0 0 distanceFromCenter, rotate = identity } )
        , ( meshL, adjustments.l, { centerPosition = Vec3.vec3 -distanceFromCenter 0 0, rotate = Mat4.rotate (degrees -90) Vec3.j } )
        , ( meshU, adjustments.u, { centerPosition = Vec3.vec3 0 distanceFromCenter 0, rotate = Mat4.rotate (degrees -90) Vec3.i } )
        , ( meshD, adjustments.d, { centerPosition = Vec3.vec3 0 -distanceFromCenter 0, rotate = Mat4.rotate (degrees 90) Vec3.i } )
        , ( meshR, adjustments.r, { centerPosition = Vec3.vec3 distanceFromCenter 0 0, rotate = Mat4.rotate (degrees 90) Vec3.j } )
        , ( meshB, adjustments.b, { centerPosition = Vec3.vec3 0 0 -distanceFromCenter, rotate = Mat4.rotate (degrees 180) Vec3.j } )
        ]


type alias MeshLetterParams =
    { height :
        Float
    , centerPosition : Vec3
    , rotate : Mat4 -> Mat4
    , color : Color
    , granularity : Float
    }


meshF : MeshLetterParams -> WebGL.Mesh Vertex
meshF params =
    let
        -- Bounding box pre-scaling is 150 (width) x 225 (height)
        boundingWidth =
            150

        boundingHeight =
            225

        strokeWidth =
            30
    in
    meshLetter params
        { boundingWidth = boundingWidth, boundingHeight = boundingHeight, strokeWidth = strokeWidth }
        [ Line
            { from = ( strokeWidth / 2, 0 )
            , to = ( strokeWidth / 2, boundingHeight )
            }
        , Line
            { from = ( 0, boundingHeight - strokeWidth / 2 )
            , to = ( boundingWidth, boundingHeight - strokeWidth / 2 )
            }
        , Line
            { from = ( 0, boundingHeight / 2 )
            , to = ( boundingWidth * 13 / 15, boundingHeight / 2 )
            }
        ]


meshL : MeshLetterParams -> WebGL.Mesh Vertex
meshL params =
    let
        -- Bounding box pre-scaling is 150 (width) x 225 (height)
        boundingWidth =
            150

        boundingHeight =
            225

        strokeWidth =
            30
    in
    meshLetter params
        { boundingWidth = boundingWidth, boundingHeight = boundingHeight, strokeWidth = strokeWidth }
        [ Line
            { from = ( strokeWidth / 2, 0 )
            , to = ( strokeWidth / 2, boundingHeight )
            }
        , Line
            { from = ( 0, strokeWidth / 2 )
            , to = ( boundingWidth, strokeWidth / 2 )
            }
        ]


meshU : MeshLetterParams -> WebGL.Mesh Vertex
meshU params =
    let
        -- Bounding box pre-scaling is 220 (width) x 300 (height)
        boundingWidth =
            220

        boundingHeight =
            300

        strokeWidth =
            35
    in
    meshLetter
        { params | rotate = params.rotate >> Mat4.rotate (degrees 180) Vec3.k }
        { boundingWidth = boundingWidth, boundingHeight = boundingHeight, strokeWidth = strokeWidth }
        [ Line
            { from = ( strokeWidth / 2, 0 )
            , to = ( strokeWidth / 2, boundingHeight * 2 / 3 )
            }
        , HalfEllipse
            { startX = strokeWidth / 2
            , endX = boundingWidth - strokeWidth / 2
            , centerYCoordinate = boundingHeight * 2 / 3
            , height = 82.5
            }
        , Line
            { from = ( boundingWidth - strokeWidth / 2, 0 )
            , to = ( boundingWidth - strokeWidth / 2, boundingHeight * 2 / 3 )
            }
        ]


meshD : MeshLetterParams -> WebGL.Mesh Vertex
meshD params =
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
            params.height

        preRotateHeight =
            preRotateWidth * boundingHeight / boundingWidth

        strokeWidth =
            35
    in
    meshLetter
        { params | rotate = params.rotate >> Mat4.rotate (degrees -90) Vec3.k, height = preRotateHeight }
        { boundingWidth = boundingWidth, boundingHeight = boundingHeight, strokeWidth = strokeWidth }
        [ Line
            { from = ( 0, strokeWidth / 2 )
            , to = ( boundingWidth, strokeWidth / 2 )
            }
        , HalfEllipse
            { startX = strokeWidth / 2
            , endX = boundingWidth - strokeWidth / 2
            , centerYCoordinate = strokeWidth
            , height = boundingHeight - strokeWidth * 3 / 2
            }
        ]


meshR : MeshLetterParams -> WebGL.Mesh Vertex
meshR params =
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
            params.height

        preRotateHeight =
            preRotateWidth * boundingHeight / boundingWidth

        strokeWidth =
            35
    in
    meshLetter
        { params | rotate = params.rotate >> Mat4.rotate (degrees -90) Vec3.k, height = preRotateHeight }
        { boundingWidth = boundingWidth, boundingHeight = boundingHeight, strokeWidth = strokeWidth }
        [ Line
            { from = ( 0, strokeWidth / 2 )
            , to = ( boundingWidth, strokeWidth / 2 )
            }
        , Line
            { from = ( strokeWidth / 2, 0 )
            , to = ( strokeWidth / 2, boundingHeight * 10 / 21 )
            }
        , HalfEllipse
            { startX = strokeWidth / 2
            , endX = boundingWidth / 2
            , centerYCoordinate = boundingHeight * 10 / 21
            , height = boundingHeight * 95 / 210
            }
        , Line
            { from = ( boundingWidth / 2, boundingHeight * 10 / 21 )
            , to = ( boundingWidth / 2, 0 )
            }
        , Line
            { from = ( boundingWidth / 2, boundingHeight * 12 / 21 )
            , to = ( boundingWidth, boundingWidth * 2 / 3 )
            }
        ]


meshB : MeshLetterParams -> WebGL.Mesh Vertex
meshB params =
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
            params.height

        preRotateHeight =
            preRotateWidth * boundingHeight / boundingWidth

        strokeWidth =
            35
    in
    meshLetter
        { params | rotate = params.rotate >> Mat4.rotate (degrees -90) Vec3.k, height = preRotateHeight }
        { boundingWidth = boundingWidth, boundingHeight = boundingHeight, strokeWidth = strokeWidth }
        [ Line
            { from = ( 0, strokeWidth / 2 )
            , to = ( boundingWidth, strokeWidth / 2 )
            }
        , Line
            { from = ( strokeWidth / 2, 0 )
            , to = ( strokeWidth / 2, boundingHeight * 10 / 23 )
            }
        , HalfEllipse
            { startX = strokeWidth / 2
            , endX = boundingWidth / 2
            , centerYCoordinate = boundingHeight * 10 / 23
            , height = boundingHeight * 95 / 255
            }
        , Line
            { from = ( boundingWidth / 2, boundingHeight * 10 / 23 )
            , to = ( boundingWidth / 2, 0 )
            }
        , HalfEllipse
            { startX = boundingWidth / 2
            , endX = boundingWidth - strokeWidth / 2
            , centerYCoordinate = boundingHeight * 10 / 23
            , height = boundingHeight * 95 / 255
            }
        , Line
            { from = ( boundingWidth - strokeWidth / 2, boundingHeight * 10 / 23 )
            , to = ( boundingWidth - strokeWidth / 2, 0 )
            }
        ]



-- LOGIC AND MAPPINGS


type LetterSegment
    = Line { from : ( Float, Float ), to : ( Float, Float ) }
    | HalfEllipse { startX : Float, endX : Float, centerYCoordinate : Float, height : Float }


meshLetter :
    MeshLetterParams
    -> { boundingWidth : Float, boundingHeight : Float, strokeWidth : Float }
    -> List LetterSegment
    -> WebGL.Mesh Vertex
meshLetter { centerPosition, height, rotate, color, granularity } { boundingWidth, boundingHeight, strokeWidth } segments =
    let
        width =
            height * boundingWidth / boundingHeight

        scale =
            height / boundingHeight

        scaleAdjustedGranularity =
            granularity / scale
    in
    segments
        |> List.map (segmentToTriangles { strokeWidth = strokeWidth, color = color, granularity = scaleAdjustedGranularity })
        |> List.concat
        |> List.map (mapTriple <| mapPosition <| Vec3.scale scale)
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


segmentToTriangles : { strokeWidth : Float, color : Color, granularity : Float } -> LetterSegment -> List ( Vertex, Vertex, Vertex )
segmentToTriangles { strokeWidth, color, granularity } segment =
    case segment of
        Line { from, to } ->
            triangleLine
                { from = tupleToVector from
                , to = tupleToVector to
                , width = strokeWidth
                , color = color
                , zCoordinate = 0
                }

        HalfEllipse { startX, endX, centerYCoordinate, height } ->
            halfEllipse
                { startX = startX
                , endX = endX
                , centerYCoordinate = centerYCoordinate
                , height = height
                , granularity = granularity
                , color = color
                , zCoordinate = 0
                , strokeWidth = strokeWidth
                }


tupleToVector : ( Float, Float ) -> Vec2
tupleToVector ( x, y ) =
    Vec2.vec2 x y


getRgb255Color : CubeTheme -> CubeColor -> Color
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


colorToVector : Color -> Vec3
colorToVector color =
    let
        rgb =
            Color.toRgba color
    in
    Vec3.vec3 rgb.red rgb.green rgb.blue


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
    = -- It was unused so it made linter error but no other reason it can't be included later
      -- XRotateDegrees Float
      YRotateDegrees Float
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


square :
    { center : Vec3
    , innerColor : Color
    , borderColor : Color
    , orthogonalPlaneDirection1 : Vec3
    , orthogonalPlaneDirection2 : Vec3
    , totalWidthAndHeight : Float
    , borderWidth : Float
    }
    -> List ( Vertex, Vertex, Vertex )
square { center, innerColor, orthogonalPlaneDirection1, orthogonalPlaneDirection2, totalWidthAndHeight, borderWidth, borderColor } =
    let
        innerVertex position =
            positionToVertex { color = innerColor } position

        borderVertex position =
            positionToVertex { color = borderColor } position

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


triangleLine : { from : Vec2, to : Vec2, zCoordinate : Float, width : Float, color : Color } -> List ( Vertex, Vertex, Vertex )
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


twoDTrianglesToColored3d : { zCoordinate : Float, color : Color } -> List ( Vec2, Vec2, Vec2 ) -> List ( Vertex, Vertex, Vertex )
twoDTrianglesToColored3d { zCoordinate, color } triangles =
    triangles
        |> List.map (mapTriple <| twoDTo3d zCoordinate)
        |> List.map (mapTriple <| positionToVertex { color = color })


twoDTo3d : Float -> Vec2 -> Vec3
twoDTo3d zCoordinate xy =
    Vec3.vec3 (Vec2.getX xy) (Vec2.getY xy) zCoordinate


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
    , color : Color
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
        { x = -width / 2, triangles = [], maybePrevStartCoordinates = Nothing }
        -- The reverse I believe is just needed to be compatible with the adding of beginning
        -- and end lines. At the time of writing I don't quite remember though, I tested removing it
        -- and it does slightly break the rendering
        |> List.reverse
        |> List.concat
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


halfEllipseHelper :
    { width : Float, height : Float, granularity : Float, strokeWidth : Float, color : Color }
    ->
        { x : Float
        , triangles : List (List ( Vertex, Vertex, Vertex ))
        , maybePrevStartCoordinates : Maybe Vec2
        }
    -> List (List ( Vertex, Vertex, Vertex ))
halfEllipseHelper params { x, triangles, maybePrevStartCoordinates } =
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

        newTriangles =
            maybePrevStartCoordinates
                |> Maybe.map
                    (\prevStartCoordinates ->
                        let
                            prevDirection =
                                Vec2.sub prevStartCoordinates startCoordinates

                            currentDirection =
                                Vec2.sub endCoordinates startCoordinates

                            topLeftCorner =
                                prevDirection
                                    |> getVec2Normal
                                    |> ensureDirectionIsYPositive
                                    |> Vec2.normalize
                                    |> Vec2.scale (params.strokeWidth / 2)
                                    |> Vec2.add startCoordinates

                            topRightCorner =
                                currentDirection
                                    |> getVec2Normal
                                    |> ensureDirectionIsYPositive
                                    |> Vec2.normalize
                                    |> Vec2.scale (params.strokeWidth / 2)
                                    |> Vec2.add startCoordinates

                            fillerTriangle =
                                ( startCoordinates, topLeftCorner, topRightCorner )
                                    |> mapTriple (twoDTo3d 0)
                                    |> mapTriple (positionToVertex { color = params.color })
                                    |> List.singleton
                        in
                        newLineSegment :: fillerTriangle :: triangles
                    )
                |> Maybe.withDefault (newLineSegment :: triangles)
    in
    if rx - Vec2.getX endCoordinates < params.granularity / 20 then
        newTriangles

    else
        halfEllipseHelper params
            { x = Vec2.getX endCoordinates
            , triangles = newTriangles
            , maybePrevStartCoordinates = Just startCoordinates
            }


getVec2Normal : Vec2 -> Vec2
getVec2Normal vector =
    Vec2.vec2 (Vec2.getY vector * -1) (Vec2.getX vector)


ensureDirectionIsYPositive : Vec2 -> Vec2
ensureDirectionIsYPositive vector =
    if Vec2.getY vector < 0 then
        Vec2.negate vector

    else
        vector


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
    if maxX - minX < targetDistance / 10 then
        getPositiveEllipseCoordinatesFromX { rx = rx, ry = ry } maxX

    else if testDistance >= targetDistance then
        binarySearchEllipseDistance { params | maxX = xToTest }

    else
        binarySearchEllipseDistance { params | minX = xToTest }


getPositiveEllipseCoordinatesFromX : { rx : Float, ry : Float } -> Float -> Vec2
getPositiveEllipseCoordinatesFromX { rx, ry } x =
    Vec2.fromRecord
        { x = x

        -- This is just from the equation of an ellipse
        , y = ry * sqrt (rx * rx - x * x) / rx
        }


addBeginningEllipseLine : { startX : Float, startY : Float, width : Float, color : Color } -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
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


addEndingEllipseLine : { startX : Float, startY : Float, width : Float, color : Color } -> List ( Vertex, Vertex, Vertex ) -> List ( Vertex, Vertex, Vertex )
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


setTransformation : Mat4 -> Vertex -> Vertex
setTransformation newTransformation oldVertex =
    { oldVertex | transformation = newTransformation }


mapPosition : (Vec3 -> Vec3) -> Vertex -> Vertex
mapPosition fn original =
    { original | position = fn original.position }


positionToVertex : { color : Color } -> Vec3 -> Vertex
positionToVertex { color } position =
    { position = position, color = colorToVector color, transformation = Mat4.identity }


mapTriple : (a -> b) -> ( a, a, a ) -> ( b, b, b )
mapTriple fn ( x, y, z ) =
    ( fn x, fn y, fn z )
