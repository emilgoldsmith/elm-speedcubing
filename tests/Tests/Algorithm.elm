module Tests.Algorithm exposing (algorithmFuzzer, appendTests, fromStringTests, inverseAlgTests, toStringTests, turnDirectionFuzzer, turnFuzzer, turnLengthFuzzer, turnableFuzzer)

{-| This represents an Algorithm, which is an ordered sequence of moves to be applied
to a cube. Enjoy!
-}

import Algorithm
import Expect
import Fuzz
import Test exposing (..)


fromStringTests : Test
fromStringTests =
    describe "fromString"
        [ fuzz validAlgorithmString "successfully parses valid algorithm strings" <|
            Algorithm.fromString
                >> Expect.ok
        , fuzz2 algorithmFuzzer turnSeparator "a rendered algorithm is correctly retrieved no matter the separator" <|
            \alg separator ->
                renderAlgorithm alg separator
                    |> Algorithm.fromString
                    |> Expect.equal (Ok alg)
        , test "handles differing whitespace separation between turns" <|
            \_ ->
                Algorithm.fromString "U U  U\tU   U  \t U    \t    U"
                    |> Expect.equal
                        (Ok <|
                            Algorithm.build <|
                                List.repeat 7 (Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise)
                        )
        , test "handles leading and trailing whitespace" <|
            \_ ->
                Algorithm.fromString "\t U \t"
                    |> Expect.ok
        , test "Confidence check that a simple example maps to what we would expect" <|
            \_ ->
                Algorithm.fromString "RU2B'"
                    |> Expect.equal
                        (Ok <|
                            Algorithm.build
                                [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
                                , Algorithm.Turn Algorithm.U Algorithm.Halfway Algorithm.Clockwise
                                , Algorithm.Turn Algorithm.B Algorithm.OneQuarter Algorithm.CounterClockwise
                                ]
                        )
        , test "handles parentheses" <|
            \_ ->
                Algorithm.fromString "(U) U (U U)"
                    |> Expect.equal
                        (Ok <|
                            Algorithm.build <|
                                List.repeat 4 (Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise)
                        )
        , fuzz obviouslyInvalidAlgorithmString "errors on invalid algorithms, but never crashes" <|
            Algorithm.fromString
                >> Expect.all
                    [ Expect.err
                    , \result ->
                        case result of
                            -- We just pass here as we are expecting an error above
                            Ok _ ->
                                Expect.pass

                            Err error ->
                                case error of
                                    Algorithm.ParserCrashed message ->
                                        Expect.fail ("ParserCrashed should never occur. The message was `" ++ message ++ "`")

                                    _ ->
                                        Expect.pass
                    ]
        , test "errors on empty string as in the real world an algorithm always has turns" <|
            \_ ->
                Algorithm.fromString ""
                    |> Expect.equal (Err Algorithm.EmptyAlgorithm)
        , test "pure whitespace errors as EmptyAlgorithm" <|
            \_ ->
                Algorithm.fromString "  \t\t \t  \t \t  "
                    |> Expect.equal (Err Algorithm.EmptyAlgorithm)
        , test "An unexpected alphabetic character errors as invalid turnable" <|
            \_ ->
                Algorithm.fromString "UB'AFM2'"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnable
                                { inputString = "UB'AFM2'"
                                , errorIndex = 3
                                , invalidTurnable = "A"
                                }
                        )
        , test "errors on 2 apostrophes in a row expecting a turnable" <|
            \_ ->
                Algorithm.fromString "U''"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnable
                                { inputString = "U''"
                                , errorIndex = 2
                                , invalidTurnable = "'"
                                }
                        )
        , test "errors on whitespace between the turnable and the apostrophe" <|
            \_ ->
                Algorithm.fromString "U  \t '"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnWouldWorkWithoutSpace
                                { inputString = "U  \t '"
                                , wrongWhitespaceStart = 1
                                , wrongWhitespaceEnd = 5
                                }
                        )
        , test "errors on space between turnable and turn length" <|
            \_ ->
                Algorithm.fromString "U \t \t 2"
                    |> Expect.equal
                        (Err <|
                            Algorithm.InvalidTurnWouldWorkWithoutSpace
                                { inputString = "U \t \t 2"
                                , wrongWhitespaceStart = 1
                                , wrongWhitespaceEnd = 6
                                }
                        )
        , test "errors on apostrophe before turn length" <|
            \_ ->
                Algorithm.fromString "U'2" |> Expect.err
        , test "errors on turn length 4" <|
            \_ ->
                Algorithm.fromString "U4" |> Expect.err
        , test "errors on turn length 1" <|
            \_ ->
                Algorithm.fromString "U1" |> Expect.err
        , test "errors on turn length specified twice" <|
            \_ ->
                Algorithm.fromString "U22'" |> Expect.err
        , test "errors on newline between turns" <|
            \_ ->
                Algorithm.fromString "U2'\nU" |> Expect.err

        -- , todo "The turnable specified twice should be tested for a good error message"
        -- Seems like the only use for that could be to specify not to double flick in a special case? But should be safe to error on that and assume it's an input error
        ]


toStringTests : Test
toStringTests =
    describe "toString"
        [ fuzz algorithmFuzzer "stringified algorithm decodes back to original value" <|
            \algorithm ->
                algorithm
                    |> Algorithm.toString
                    |> Algorithm.fromString
                    |> Expect.equal (Ok algorithm)
        , test "Passes specific case that tries covering all types of turnables, lengths and directions" <|
            \_ ->
                Algorithm.build
                    [ Algorithm.Turn
                        Algorithm.U
                        Algorithm.OneQuarter
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.D
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.F
                        Algorithm.Halfway
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.B
                        Algorithm.OneQuarter
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.R
                        Algorithm.ThreeQuarters
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.L
                        Algorithm.ThreeQuarters
                        Algorithm.CounterClockwise
                    , Algorithm.Turn
                        Algorithm.M
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.S
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.E
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.X
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.Y
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    , Algorithm.Turn
                        Algorithm.Z
                        Algorithm.Halfway
                        Algorithm.Clockwise
                    ]
                    |> Algorithm.toString
                    |> Expect.equal "U D2 F2' B' R3 L3' M2 S2 E2 x2 y2 z2"
        , test "handles empty algorithm" <|
            \_ ->
                Algorithm.empty
                    |> Algorithm.toString
                    |> Expect.equal ""
        ]


inverseAlgTests : Test
inverseAlgTests =
    describe "inverseAlg"
        [ fuzz algorithmFuzzer "the inverse of the inverse should be the original algorithm" <|
            \alg ->
                alg
                    |> Algorithm.inverse
                    |> Algorithm.inverse
                    |> Expect.equal alg
        , fuzz2 algorithmFuzzer algorithmFuzzer "the inverse of an algorithm equals splitting the alg in two, inversing each part and swapping their order" <|
            \part1 part2 ->
                Algorithm.appendTo (Algorithm.inverse part2) (Algorithm.inverse part1)
                    |> Expect.equal (Algorithm.inverse (Algorithm.appendTo part1 part2))
        , test "correctly inverses simple example" <|
            \_ ->
                let
                    alg =
                        Algorithm.build
                            [ Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
                            , Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.CounterClockwise
                            ]

                    inversedAlg =
                        Algorithm.build
                            [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
                            , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.CounterClockwise
                            ]
                in
                alg |> Algorithm.inverse |> Expect.equal inversedAlg
        ]


appendTests : Test
appendTests =
    describe "appenders"
        [ describe "appendTo"
            [ fuzz2 turnFuzzer turnFuzzer "Appending two algorithms each consisting of a turn equals an algorithm with those two turns in a row" <|
                \turn1 turn2 ->
                    Algorithm.appendTo (Algorithm.build [ turn1 ]) (Algorithm.build [ turn2 ])
                        |> Expect.equal (Algorithm.build [ turn1, turn2 ])
            , fuzz algorithmFuzzer "Appending to an empty algorithm equals the second algorithm" <|
                \algorithm ->
                    Algorithm.appendTo Algorithm.empty algorithm
                        |> Expect.equal algorithm
            , fuzz algorithmFuzzer "Appending an empty algorithm to an algorithm equals the first algorithm" <|
                \algorithm ->
                    Algorithm.appendTo algorithm Algorithm.empty
                        |> Expect.equal algorithm
            , test "Appending two empty algorithm equals an empty algorithm" <|
                \_ ->
                    Algorithm.appendTo Algorithm.empty Algorithm.empty
                        |> Expect.equal Algorithm.empty
            ]
        , describe "append"
            [ fuzz2 algorithmFuzzer algorithmFuzzer "is the opposite of appendTo" <|
                \alg1 alg2 ->
                    Algorithm.append alg1 alg2
                        |> Expect.equal (Algorithm.appendTo alg2 alg1)
            , fuzz algorithmFuzzer "Appending an empty algorithm equals the second algorithm" <|
                \algorithm ->
                    Algorithm.append Algorithm.empty algorithm
                        |> Expect.equal algorithm
            , fuzz algorithmFuzzer "Appending an algorithm to an empty algorithm equals the first algorithm" <|
                \algorithm ->
                    Algorithm.append algorithm Algorithm.empty
                        |> Expect.equal algorithm
            , test "Appending two empty algorithm equals an empty algorithm" <|
                \_ ->
                    Algorithm.append Algorithm.empty Algorithm.empty
                        |> Expect.equal Algorithm.empty
            ]
        ]


obviouslyInvalidAlgorithmString : Fuzz.Fuzzer String
obviouslyInvalidAlgorithmString =
    let
        notFaceOrSlice c =
            not <| List.member c (List.map renderTurnable Algorithm.allTurnables)

        removeFaceAndSliceChars =
            String.filter notFaceOrSlice
    in
    Fuzz.map removeFaceAndSliceChars Fuzz.string


validAlgorithmString : Fuzz.Fuzzer String
validAlgorithmString =
    Fuzz.map2 renderAlgorithm algorithmFuzzer turnSeparator


algorithmFuzzer : Fuzz.Fuzzer Algorithm.Algorithm
algorithmFuzzer =
    let
        nonEmptyTurnList =
            Fuzz.map2 (::) turnFuzzer <| Fuzz.list turnFuzzer
    in
    Fuzz.map Algorithm.build nonEmptyTurnList


renderAlgorithm : Algorithm.Algorithm -> String -> String
renderAlgorithm alg separator =
    let
        renderedTurnList =
            Algorithm.toTurnList >> List.map renderTurn <| alg
    in
    String.join separator renderedTurnList


turnSeparator : Fuzz.Fuzzer String
turnSeparator =
    Fuzz.oneOf
        [ Fuzz.constant ""
        , Fuzz.constant " "
        , Fuzz.constant "  "
        , Fuzz.constant "   "
        , Fuzz.constant "\t"
        ]


turnFuzzer : Fuzz.Fuzzer Algorithm.Turn
turnFuzzer =
    Fuzz.map3 Algorithm.Turn turnableFuzzer turnLengthFuzzer turnDirectionFuzzer


renderTurn : Algorithm.Turn -> String
renderTurn (Algorithm.Turn x length direction) =
    -- For double/triple turns clockwise we format it as U2' / U3' as these are used in some
    -- algorithms for explanations of fingertricks, also notice it's not U'2 or U'3. This
    -- decision was made based on "use in the wild" specifically the Youtuber Jperm's use.
    String.fromChar (renderTurnable x) ++ renderLength length ++ renderDirection direction


turnableFuzzer : Fuzz.Fuzzer Algorithm.Turnable
turnableFuzzer =
    Fuzz.oneOf <| List.map Fuzz.constant Algorithm.allTurnables


renderTurnable : Algorithm.Turnable -> Char
renderTurnable x =
    case x of
        Algorithm.U ->
            'U'

        Algorithm.D ->
            'D'

        Algorithm.L ->
            'L'

        Algorithm.R ->
            'R'

        Algorithm.F ->
            'F'

        Algorithm.B ->
            'B'

        Algorithm.M ->
            'M'

        Algorithm.S ->
            'S'

        Algorithm.E ->
            'E'

        Algorithm.X ->
            'x'

        Algorithm.Y ->
            'y'

        Algorithm.Z ->
            'z'


turnLengthFuzzer : Fuzz.Fuzzer Algorithm.TurnLength
turnLengthFuzzer =
    Fuzz.oneOf <| List.map Fuzz.constant Algorithm.allTurnLengths


renderLength : Algorithm.TurnLength -> String
renderLength length =
    case length of
        Algorithm.OneQuarter ->
            ""

        Algorithm.Halfway ->
            "2"

        Algorithm.ThreeQuarters ->
            "3"


turnDirectionFuzzer : Fuzz.Fuzzer Algorithm.TurnDirection
turnDirectionFuzzer =
    Fuzz.oneOf <| List.map Fuzz.constant Algorithm.allTurnDirections


renderDirection : Algorithm.TurnDirection -> String
renderDirection dir =
    case dir of
        Algorithm.Clockwise ->
            ""

        Algorithm.CounterClockwise ->
            "'"
