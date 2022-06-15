module PLLTwoSidedRecognitionPatterns exposing (main)

import AUF
import Algorithm
import Cube
import Html exposing (..)
import Html.Attributes exposing (..)
import List.Nonempty
import PLL


main : Html msg
main =
    div []
        (PLL.all
            |> List.Nonempty.toList
            |> List.map
                (\pll ->
                    div [ style "display" "flex", style "flex-direction" "row", style "margin" "40px" ]
                        ([ div [ style "align-self" "center" ] [ text (PLL.getLetters pll ++ ": ") ]
                         ]
                            ++ (AUF.all
                                    |> List.Nonempty.toList
                                    |> List.map
                                        (\preAUF ->
                                            div []
                                                [ text <|
                                                    explainPLLRecognitionPattern <|
                                                        PLL.getUniqueTwoSidedRecognitionPattern algorithms ( preAUF, pll, AUF.None )
                                                , Cube.view []
                                                    { pixelSize = 300
                                                    , displayAngle = Cube.ufrDisplayAngle
                                                    , annotateFaces = False
                                                    }
                                                  <|
                                                    Cube.applyAlgorithm
                                                        (Algorithm.inverse <|
                                                            Algorithm.append (AUF.toAlgorithm preAUF) (PLL.getAlgorithm algorithms pll)
                                                        )
                                                        Cube.solved
                                                ]
                                        )
                               )
                        )
                )
        )


explainPLLRecognitionPattern : PLL.PLLRecognitionPattern -> String
explainPLLRecognitionPattern pattern =
    ""


type alias ParsedPattern =
    { interestingCharacteristics : List PLL.PLLRecognitionCharacteristic
    , oppositeColoreds : Maybe ( List PLL.PLLRecognitionCharacteristic, List PLL.PLLRecognitionCharacteristic )
    }


emptyParsedPattern : ParsedPattern
emptyParsedPattern =
    { interestingCharacteristics = []
    , oppositeColoreds = Nothing
    }


combineParsedPatterns : ParsedPattern -> ParsedPattern -> ParsedPattern
combineParsedPatterns a b =
    { interestingCharacteristics = a.interestingCharacteristics ++ b.interestingCharacteristics
    , oppositeColoreds =
        a.oppositeColoreds
            |> Maybe.map Just
            |> Maybe.withDefault b.oppositeColoreds
    }


parsePattern : PLL.PLLRecognitionPattern -> ParsedPattern
parsePattern pattern =
    case pattern of
        PLL.Patterns ( a, b ) ->
            combineParsedPatterns (parsePattern a) (parsePattern b)

        PLL.Characteristic characteristic ->
            { emptyParsedPattern | interestingCharacteristics = [ characteristic ] }

        PLL.OppositeColors ( a, b ) ->
            let
                firstParsedPattern =
                    parsePattern a

                secondParsedPattern =
                    parsePattern b

                combinedParsedPattern =
                    combineParsedPatterns firstParsedPattern secondParsedPattern
            in
            { combinedParsedPattern
                | oppositeColoreds =
                    Just
                        ( firstParsedPattern.interestingCharacteristics
                        , secondParsedPattern.interestingCharacteristics
                        )
            }

        _ ->
            emptyParsedPattern


algorithms : PLL.Algorithms
algorithms =
    PLL.referenceAlgorithms
