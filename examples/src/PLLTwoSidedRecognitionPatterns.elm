module PLLTwoSidedRecognitionPatterns exposing (main)

import AUF
import Algorithm exposing (Algorithm)
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
            |> List.filter (\pll -> pll == PLL.T || pll == PLL.Ga)
            |> List.map
                (\pll ->
                    div [ style "display" "flex", style "flex-direction" "row", style "margin" "40px" ]
                        ([ div [ style "align-self" "center" ] [ text (PLL.getLetters pll ++ ": ") ]
                         ]
                            ++ (AUF.all
                                    |> List.Nonempty.toList
                                    |> List.map
                                        (\preAUF ->
                                            div
                                                [ style "max-width" "300px"
                                                , style "margin-right" "20px"
                                                ]
                                                [ text <|
                                                    explainPLLRecognitionPattern <|
                                                        PLL.getUniqueTwoSidedRecognitionPattern algorithms ( Debug.log "preAUF" preAUF, Debug.log "pll" pll, AUF.None )
                                                , Cube.view []
                                                    { pixelSize = 300
                                                    , displayAngle = Cube.ufrDisplayAngle
                                                    , annotateFaces = False
                                                    }
                                                  <|
                                                    Cube.applyAlgorithm
                                                        (log <|
                                                            Algorithm.inverse <|
                                                                Algorithm.append (AUF.toAlgorithm preAUF) (PLL.getAlgorithm algorithms pll)
                                                        )
                                                        Cube.solved
                                                ]
                                        )
                               )
                        )
                )
        )


log : Algorithm -> Algorithm
log x =
    let
        _ =
            Debug.log "algorithm" <| Algorithm.toString x
    in
    x


explainPLLRecognitionPattern : PLL.PLLRecognitionPattern -> String
explainPLLRecognitionPattern pattern =
    let
        { characteristics, oppositeColoreds, sameColoreds, differentColoreds, noOtherStickersMatchThanThese } =
            Debug.log "pattern" <| parsePattern pattern

        interestingCharacteristics =
            List.filter isInterestingCharacteristic characteristics
    in
    "There "
        ++ (if
                List.head interestingCharacteristics
                    |> Maybe.map isPlural
                    |> Maybe.withDefault False
            then
                "are"

            else
                "is"
           )
        ++ " "
        ++ toSentenceList
            { indefinite = True }
            interestingCharacteristics
        ++ (case oppositeColoreds of
                Nothing ->
                    ""

                Just ( first, second ) ->
                    ". "
                        ++ capitalize (toSentenceList { indefinite = False } first)
                        ++ " "
                        ++ (if isWholeSentencePlural first then
                                "are"

                            else
                                "is"
                           )
                        ++ " opposite colors of "
                        ++ toSentenceList { indefinite = False } second
           )
        ++ (case sameColoreds of
                Nothing ->
                    ""

                Just ( first, second ) ->
                    ". "
                        ++ capitalize (characteristicToString { indefinite = False } first)
                        ++ " "
                        ++ (if isPlural first then
                                "are"

                            else
                                "is"
                           )
                        ++ " the same color as "
                        ++ characteristicToString { indefinite = False } second
           )
        ++ (if List.length differentColoreds < 2 then
                ""

            else
                ". "
                    ++ capitalize (toSentenceList { indefinite = False } differentColoreds)
                    ++ " are all individually different colors from each other"
           )
        ++ (if List.isEmpty noOtherStickersMatchThanThese then
                ""

            else
                ". There are no other stickers with matching colors other than "
                    ++ toSentenceList { indefinite = False } noOtherStickersMatchThanThese
           )


capitalize : String -> String
capitalize s =
    String.toUpper (String.left 1 s) ++ String.dropLeft 1 s


toSentenceList : { indefinite : Bool } -> List PLL.PLLRecognitionCharacteristic -> String
toSentenceList { indefinite } list =
    case list of
        [] ->
            ""

        [ x ] ->
            characteristicToString { indefinite = indefinite } x

        [ x, y ] ->
            characteristicToString { indefinite = indefinite } x ++ " and " ++ characteristicToString { indefinite = indefinite } y

        x :: xs ->
            characteristicToString { indefinite = indefinite } x ++ ", " ++ toSentenceList { indefinite = indefinite } xs


isWholeSentencePlural : List PLL.PLLRecognitionCharacteristic -> Bool
isWholeSentencePlural characteristics =
    case characteristics of
        [] ->
            False

        [ x ] ->
            isPlural x

        _ ->
            True


type alias ParsedPattern =
    { characteristics : List PLL.PLLRecognitionCharacteristic
    , oppositeColoreds : Maybe ( List PLL.PLLRecognitionCharacteristic, List PLL.PLLRecognitionCharacteristic )
    , sameColoreds : Maybe ( PLL.PLLRecognitionCharacteristic, PLL.PLLRecognitionCharacteristic )
    , differentColoreds : List PLL.PLLRecognitionCharacteristic
    , noOtherStickersMatchThanThese : List PLL.PLLRecognitionCharacteristic
    }


emptyParsedPattern : ParsedPattern
emptyParsedPattern =
    { characteristics = []
    , oppositeColoreds = Nothing
    , sameColoreds = Nothing
    , differentColoreds = []
    , noOtherStickersMatchThanThese = []
    }


combineParsedPatterns : ParsedPattern -> ParsedPattern -> ParsedPattern
combineParsedPatterns a b =
    { characteristics = a.characteristics ++ b.characteristics
    , oppositeColoreds =
        a.oppositeColoreds
            |> Maybe.map Just
            |> Maybe.withDefault b.oppositeColoreds
    , sameColoreds =
        a.sameColoreds
            |> Maybe.map Just
            |> Maybe.withDefault b.sameColoreds
    , differentColoreds =
        if
            List.length a.differentColoreds
                >= List.length b.differentColoreds
        then
            a.differentColoreds

        else
            b.differentColoreds
    , noOtherStickersMatchThanThese =
        if
            List.length a.noOtherStickersMatchThanThese
                >= List.length b.noOtherStickersMatchThanThese
        then
            a.noOtherStickersMatchThanThese

        else
            b.noOtherStickersMatchThanThese
    }


parsePattern : PLL.PLLRecognitionPattern -> ParsedPattern
parsePattern pattern =
    case pattern of
        PLL.Patterns ( a, b ) ->
            combineParsedPatterns (parsePattern a) (parsePattern b)

        PLL.Characteristic characteristic ->
            { emptyParsedPattern
                | characteristics = [ characteristic ]
            }

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
                        ( firstParsedPattern.characteristics
                        , secondParsedPattern.characteristics
                        )
            }

        PLL.SameColor ( a, b ) ->
            let
                firstParsedPattern =
                    parsePattern a

                secondParsedPattern =
                    parsePattern b

                combinedParsedPattern =
                    combineParsedPatterns firstParsedPattern secondParsedPattern
            in
            { combinedParsedPattern
                | sameColoreds =
                    Maybe.map2 Tuple.pair (List.head firstParsedPattern.characteristics) (List.head secondParsedPattern.characteristics)
            }

        PLL.DifferentColors ( a, b ) ->
            let
                firstParsedPattern =
                    parsePattern a

                secondParsedPattern =
                    parsePattern b

                combinedParsedPattern =
                    combineParsedPatterns firstParsedPattern secondParsedPattern
            in
            { combinedParsedPattern
                | differentColoreds =
                    firstParsedPattern.characteristics ++ secondParsedPattern.characteristics
            }

        PLL.NoOtherStickersMatchThanThese x ->
            let
                parsedPattern =
                    parsePattern x
            in
            { parsedPattern
                | noOtherStickersMatchThanThese = parsedPattern.characteristics
            }

        _ ->
            emptyParsedPattern


algorithms : PLL.Algorithms
algorithms =
    PLL.referenceAlgorithms


isInterestingCharacteristic : PLL.PLLRecognitionCharacteristic -> Bool
isInterestingCharacteristic characteristic =
    case characteristic of
        PLL.LeftHeadlights ->
            True

        PLL.RightHeadlights ->
            True

        PLL.RightOutsideTwoBar ->
            True

        PLL.LeftOutsideTwoBar ->
            True

        PLL.RightInsideTwoBar ->
            True

        PLL.LeftInsideTwoBar ->
            True

        PLL.RightFourChecker ->
            True

        PLL.Bookends ->
            True

        PLL.FirstStickerFromLeft ->
            False

        PLL.SecondStickerFromLeft ->
            False

        PLL.ThirdStickerFromLeft ->
            False

        PLL.FirstStickerFromRight ->
            False

        PLL.SecondStickerFromRight ->
            False

        PLL.ThirdStickerFromRight ->
            False

        _ ->
            False


characteristicToString : { indefinite : Bool } -> PLL.PLLRecognitionCharacteristic -> String
characteristicToString { indefinite } characteristic =
    let
        { indefiniteArticle, object } =
            case characteristic of
                PLL.Bookends ->
                    { indefiniteArticle = Nothing, object = "bookends" }

                PLL.RightFourChecker ->
                    { indefiniteArticle = Just "a", object = "four checker pattern on the right" }

                PLL.LeftHeadlights ->
                    { indefiniteArticle = Nothing, object = "headlights on the left" }

                PLL.RightHeadlights ->
                    { indefiniteArticle = Nothing, object = "headlights on the right" }

                PLL.RightOutsideTwoBar ->
                    { indefiniteArticle = Just "an", object = "outside two-bar on the right" }

                PLL.LeftOutsideTwoBar ->
                    { indefiniteArticle = Just "an", object = "outside two-bar on the left" }

                PLL.FirstStickerFromLeft ->
                    { indefiniteArticle = Nothing, object = "first sticker from the left" }

                PLL.FirstStickerFromRight ->
                    { indefiniteArticle = Nothing, object = "first sticker from the right" }

                PLL.SecondStickerFromLeft ->
                    { indefiniteArticle = Nothing, object = "second sticker from the left" }

                PLL.SecondStickerFromRight ->
                    { indefiniteArticle = Nothing, object = "second sticker from the right" }

                PLL.ThirdStickerFromLeft ->
                    { indefiniteArticle = Nothing, object = "third sticker from the left" }

                PLL.ThirdStickerFromRight ->
                    { indefiniteArticle = Nothing, object = "third sticker from the right" }

                PLL.RightInsideTwoBar ->
                    { indefiniteArticle = Just "an", object = "inside two-bar on the right" }

                PLL.LeftInsideTwoBar ->
                    { indefiniteArticle = Just "an", object = "inside two-bar on the left" }

                _ ->
                    { indefiniteArticle = Nothing, object = "TODO" }
    in
    if indefinite then
        (indefiniteArticle
            |> Maybe.map (\x -> x ++ " ")
            |> Maybe.withDefault ""
        )
            ++ object

    else
        "the " ++ object


isPlural : PLL.PLLRecognitionCharacteristic -> Bool
isPlural characteristic =
    case characteristic of
        PLL.RightFourChecker ->
            False

        PLL.Bookends ->
            True

        PLL.LeftHeadlights ->
            True

        PLL.RightHeadlights ->
            True

        PLL.RightOutsideTwoBar ->
            False

        PLL.LeftOutsideTwoBar ->
            False

        PLL.FirstStickerFromLeft ->
            False

        PLL.FirstStickerFromRight ->
            False

        PLL.SecondStickerFromLeft ->
            False

        PLL.SecondStickerFromRight ->
            False

        PLL.ThirdStickerFromLeft ->
            False

        PLL.ThirdStickerFromRight ->
            False

        PLL.RightInsideTwoBar ->
            False

        PLL.LeftInsideTwoBar ->
            False

        _ ->
            False
