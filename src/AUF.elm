module AUF exposing
    ( AUF(..), all
    , toAlgorithm, toAlgorithmWithCustomTurnable, toString, FromStringError(..), debugFromStringError, fromString, addToAlgorithm, fromAlgorithm, add
    )

{-| Types and helpers to deal with Adjust U Face (AUF), which
are the moves needed either to adjust the U face to the right
angle for executing your algorithms for several algorithm sets
such as OLL and PLL, or the last move needed to solve the cube
for example after PLL. See
<https://www.speedsolving.com/wiki/index.php/AUF>
for more information


# Definition and Constructors

@docs AUF, all


# Helpers

@docs toAlgorithm, toAlgorithmWithCustomTurnable, toString, FromStringError, debugFromStringError, fromString, addToAlgorithm, fromAlgorithm, add

-}

import Algorithm exposing (Algorithm)
import Cube
import Cube.Advanced
import List.Nonempty
import Utils.Enumerator



-- DEFINITION AND CONSTRUCTORS


{-| The 4 different AUFs. U, U', U2, and nothing.
Use these value constructors together with [@all](#all)
when you need to construct in different ways.
-}
type AUF
    = None
    | Clockwise
    | Halfway
    | CounterClockwise


{-| A nonempty list containing all the possible aufs.
Could for example be used to generate a random auf or
generate all the possible versions of an algorithm

    import List.Nonempty

    -- They are all there!
    List.Nonempty.length all --> 4

    -- Generate a random one
    List.Nonempty.sample all

    -- Get all versions of a Y perm
    all
        |> List.Nonempty.map (Tuple.pair PLL.Y)
        |> List.Nonempty.map (\(pll, postAuf) ->
            List.Nonempty.map
                (\preAuf -> (preAuf, pll, postAuf))
                all
        )

-}
all : List.Nonempty.Nonempty AUF
all =
    let
        fromNone auf =
            case auf of
                None ->
                    Just Clockwise

                Clockwise ->
                    Just Halfway

                Halfway ->
                    Just CounterClockwise

                CounterClockwise ->
                    Nothing
    in
    Utils.Enumerator.from None fromNone



-- HELPERS


{-| Get the algorithm that corresponds to the AUF

    import Algorithm

    toAlgorithm Halfway
    -->  Algorithm.fromTurnList
    -->    [ Algorithm.Turn
    -->        Algorithm.U
    -->        Algorithm.Halfway
    -->        Algorithm.Clockwise
    -->    ]

-}
toAlgorithm : AUF -> Algorithm
toAlgorithm =
    toAlgorithmWithCustomTurnable Algorithm.U


{-| Get the algorithm that corresponds to the AUF, but
be able to specify which turnable to use to do the AUF.
This is especially relevant for usecases such as an
algorithm does an x or z rotation (or wide move) in
the algorithm so the AUF is actually executed on a non-U
face

    import Algorithm

    toAlgorithmWithCustomTurnable Algorithm.B Halfway
    -->  Algorithm.fromTurnList
    -->    [ Algorithm.Turn
    -->        Algorithm.B
    -->        Algorithm.Halfway
    -->        Algorithm.Clockwise
    -->    ]

-}
toAlgorithmWithCustomTurnable : Algorithm.Turnable -> AUF -> Algorithm
toAlgorithmWithCustomTurnable turnable auf =
    case auf of
        None ->
            Algorithm.empty

        Clockwise ->
            Algorithm.fromTurnList
                [ Algorithm.Turn
                    turnable
                    Algorithm.OneQuarter
                    Algorithm.Clockwise
                ]

        Halfway ->
            Algorithm.fromTurnList
                [ Algorithm.Turn
                    turnable
                    Algorithm.Halfway
                    Algorithm.Clockwise
                ]

        CounterClockwise ->
            Algorithm.fromTurnList
                [ Algorithm.Turn
                    turnable
                    Algorithm.OneQuarter
                    Algorithm.CounterClockwise
                ]


{-| Get an algorithm string representation of the AUF

    toString Halfway --> "U2"

-}
toString : AUF -> String
toString =
    toAlgorithm >> Algorithm.toString


{-| Explains an issue that occurred while parsing an AUF

**InvalidAUFAlgorithm**: The algorithm was parsed correctly but was not
either an empty move or a turn of the U face

**AlgorithmParsingProblem**: The string was not a valid algorithm string
and the contained [Algorithm.FromStringError](Algorithm#FromStringError)
has the problem with the string

-}
type FromStringError
    = InvalidAUFAlgorithm String
    | AlgorithmParsingError Algorithm.FromStringError


{-| Describes the error as a string, and is not recommended
to be displayed to end-users, but rather to be used in error
mesages logged for developer's eyes etc.

    case fromString aufString of
        Ok _ ->
            doSomethingOnSuccess

        Err error ->
            logError (debugFromStringError error)

-}
debugFromStringError : FromStringError -> String
debugFromStringError error =
    case error of
        InvalidAUFAlgorithm inputString ->
            "The input string was a valid algorithm "
                ++ "but did not describe one of the 4 valid AUF possibilities: "
                ++ inputString

        AlgorithmParsingError algError ->
            Algorithm.debugFromStringError algError


{-| Attempts to parse an algorithmic representation of an AUF

    fromString "U'" --> Ok CounterClockwise

    fromString "" --> Ok None

    fromString "U B"
    --> Err (InvalidAUFAlgorithm "U B")

-}
fromString : String -> Result FromStringError AUF
fromString stringValue =
    if
        -- Algorithm.fromString doesn't accept empty strings
        -- at the time of writing because it doesn't make sense
        -- from a user perspective. So we do handle that case here
        -- as AUFs can indeed be an empty string
        stringValue
            |> String.filter
                (\x -> x /= ' ' && x /= '\t')
            |> String.isEmpty
    then
        Ok None

    else
        stringValue
            |> Algorithm.fromString
            |> Result.mapError AlgorithmParsingError
            |> Result.map algorithmToAuf
            |> Result.andThen
                (Result.fromMaybe (InvalidAUFAlgorithm stringValue))


algorithmToAuf : Algorithm -> Maybe AUF
algorithmToAuf algorithm =
    case Algorithm.toTurnList algorithm of
        [] ->
            Just None

        [ Algorithm.Turn Algorithm.U length direction ] ->
            case ( length, direction ) of
                ( Algorithm.OneQuarter, Algorithm.Clockwise ) ->
                    Just Clockwise

                ( Algorithm.Halfway, Algorithm.Clockwise ) ->
                    Just Halfway

                ( Algorithm.ThreeQuarters, Algorithm.Clockwise ) ->
                    Just CounterClockwise

                ( Algorithm.OneQuarter, Algorithm.CounterClockwise ) ->
                    Just CounterClockwise

                ( Algorithm.Halfway, Algorithm.CounterClockwise ) ->
                    Just Halfway

                ( Algorithm.ThreeQuarters, Algorithm.CounterClockwise ) ->
                    Just Clockwise

        _ ->
            Nothing


{-| Add a pre and postAUF to an algorithm outputting the resulting algorithm. This
also supports algorithms that change orientation. So for example this would do the AUF
on R instead of on U for the postAUF

    import Algorithm

    addToAlgorithm
        ( Halfway, Clockwise )
        (Algorithm.fromTurnList [ Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise ])

    --> Algorithm.fromTurnList
    -->     [ Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
    -->     , Algorithm.Turn Algorithm.Z Algorithm.OneQuarter Algorithm.Clockwise
    -->     , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
    -->     ]

-}
addToAlgorithm : ( AUF, AUF ) -> Algorithm -> Algorithm
addToAlgorithm ( preAUF, postAUF ) algorithm =
    let
        -- Getting the center cubies of the state of a cube where the algorithm
        -- was applied to a solved cube so we can determine the final orientation
        { u, f, b, l, r, d } =
            Cube.Advanced.render (Cube.applyAlgorithm algorithm Cube.solved)

        postAUFTurnable =
            [ ( u.u, Algorithm.U )
            , ( d.d, Algorithm.D )
            , ( f.f, Algorithm.F )
            , ( b.b, Algorithm.B )
            , ( l.l, Algorithm.L )
            , ( r.r, Algorithm.R )
            ]
                |> List.filter (Tuple.first >> (==) Cube.Advanced.UpColor)
                |> List.head
                |> Maybe.map Tuple.second
                |> Maybe.withDefault Algorithm.U
    in
    Algorithm.append (toAlgorithm preAUF) <|
        Algorithm.append algorithm <|
            toAlgorithmWithCustomTurnable postAUFTurnable postAUF


{-| Parses an algorithm to see if it matches a single AUF.
Note that it does not parse an algorithm with AUFs in it, it is
meant to be the inverse of toAlgorithm, so any algorithm with length
larger than 1 would return Nothing here, so it also only accepts
AUFs that use U as the turnable
-}
fromAlgorithm : Algorithm -> Maybe AUF
fromAlgorithm algorithm =
    case Algorithm.toTurnList algorithm of
        [] ->
            Just None

        [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise ] ->
            Just Clockwise

        [ Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise ] ->
            Just Halfway

        [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise ] ->
            Just CounterClockwise

        [ Algorithm.Turn Algorithm.U Algorithm.ThreeQuarters Algorithm.Clockwise ] ->
            Just CounterClockwise

        [ Algorithm.Turn Algorithm.U Algorithm.ThreeQuarters Algorithm.CounterClockwise ] ->
            Just Clockwise

        [ Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.CounterClockwise ] ->
            Just Halfway

        _ ->
            Nothing


{-| Adds together two AUFs, giving the AUF that would be the result
of applying the two AUFs consecutively
-}
add : AUF -> AUF -> AUF
add first second =
    case ( first, second ) of
        ( None, x ) ->
            x

        ( x, None ) ->
            x

        ( Clockwise, Clockwise ) ->
            Halfway

        ( Clockwise, Halfway ) ->
            CounterClockwise

        ( Clockwise, CounterClockwise ) ->
            None

        ( Halfway, Clockwise ) ->
            CounterClockwise

        ( Halfway, Halfway ) ->
            None

        ( Halfway, CounterClockwise ) ->
            Clockwise

        ( CounterClockwise, Clockwise ) ->
            None

        ( CounterClockwise, Halfway ) ->
            Clockwise

        ( CounterClockwise, CounterClockwise ) ->
            Halfway
