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
                                                        PLL.getUniqueTwoSidedRecognitionSpecification algorithms ( Debug.log "preAUF" preAUF, Debug.log "pll" pll, AUF.None )
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


explainPLLRecognitionPattern : PLL.RecognitionSpecification -> String
explainPLLRecognitionPattern { patterns, absentPatterns, oppositelyColored, adjacentlyColored, identicallyColored, differentlyColored, noOtherStickersMatchThanThese } =
    let
        parts =
            [ patterns
                |> Maybe.map
                    (\patterns_ ->
                        "The easily identifiable pattern"
                            ++ (if List.Nonempty.length patterns_ > 1 then
                                    "s are"

                                else
                                    " is"
                               )
                            ++ " "
                            ++ nonemptyToSentenceList
                                { article = Definite, finalConjunction = And }
                                (List.Nonempty.map PLL.Pattern patterns_)
                    )
            , absentPatterns
                |> Maybe.map
                    (\patterns_ ->
                        "There "
                            ++ (if
                                    (List.Nonempty.length patterns_ > 1)
                                        || isPlural (PLL.Pattern <| List.Nonempty.head patterns_)
                                then
                                    "are"

                                else
                                    "is"
                               )
                            ++ " no "
                            ++ nonemptyToSentenceList
                                { article = NoArticle, finalConjunction = Or }
                                (List.Nonempty.map PLL.Pattern patterns_)
                    )
            , oppositelyColored
                |> Maybe.map
                    (\( first, second ) ->
                        nonemptyToSentenceList { article = Definite, finalConjunction = And } first
                            ++ " "
                            ++ (if
                                    (List.Nonempty.length first > 1)
                                        || isPlural (List.Nonempty.head first)
                                then
                                    "are"

                                else
                                    "is"
                               )
                            ++ " opposite colors of "
                            ++ nonemptyToSentenceList { article = Definite, finalConjunction = And } second
                    )
            , adjacentlyColored
                |> Maybe.map
                    (\( first, second ) ->
                        nonemptyToSentenceList { article = Definite, finalConjunction = And } first
                            ++ " "
                            ++ (if
                                    (List.Nonempty.length first > 1)
                                        || isPlural (List.Nonempty.head first)
                                then
                                    "are"

                                else
                                    "is"
                               )
                            ++ " adjacent colors of "
                            ++ nonemptyToSentenceList { article = Definite, finalConjunction = And } second
                    )
            , identicallyColored
                |> Maybe.map
                    (\elements ->
                        minLength2ToSentenceList { article = Definite, finalConjunction = And } elements
                            ++ " are "
                            ++ (if lengthOfMinLength2List elements > 2 then
                                    "all "

                                else
                                    ""
                               )
                            ++ "the same color"
                    )
            , differentlyColored
                |> Maybe.map
                    (\elements ->
                        minLength2ToSentenceList { article = Definite, finalConjunction = And } elements
                            ++ " are "
                            ++ (if lengthOfMinLength2List elements > 2 then
                                    "all "

                                else
                                    ""
                               )
                            ++ "different colors from each other"
                    )
            , noOtherStickersMatchThanThese
                |> Maybe.map
                    (\elements ->
                        "There are no other stickers that match the color of any other sticker than "
                            ++ nonemptyToSentenceList { article = Definite, finalConjunction = And } elements
                    )
            ]
    in
    (parts
        |> List.filterMap identity
        |> List.map capitalize
        |> String.join ". "
    )
        ++ "."


capitalize : String -> String
capitalize s =
    String.toUpper (String.left 1 s) ++ String.dropLeft 1 s


type Article
    = NoArticle
    | Definite
    | Indefinite


type Conjunction
    = And
    | Or


toSentenceList : { article : Article, finalConjunction : Conjunction } -> List PLL.RecognitionElement -> String
toSentenceList { article, finalConjunction } list =
    case list of
        [] ->
            ""

        [ x ] ->
            elementToString { article = article } x

        [ x, y ] ->
            let
                conjunctionString =
                    case finalConjunction of
                        And ->
                            "and"

                        Or ->
                            "or"
            in
            elementToString { article = article } x
                ++ " "
                ++ conjunctionString
                ++ " "
                ++ elementToString { article = article } y

        x :: xs ->
            elementToString { article = article } x ++ ", " ++ toSentenceList { article = article, finalConjunction = finalConjunction } xs


nonemptyToSentenceList : { article : Article, finalConjunction : Conjunction } -> List.Nonempty.Nonempty PLL.RecognitionElement -> String
nonemptyToSentenceList args =
    List.Nonempty.toList >> toSentenceList args


minLength2ToSentenceList : { article : Article, finalConjunction : Conjunction } -> ( PLL.RecognitionElement, PLL.RecognitionElement, List PLL.RecognitionElement ) -> String
minLength2ToSentenceList args ( first, second, rest ) =
    toSentenceList args (first :: second :: rest)


lengthOfMinLength2List : ( a, a, List a ) -> Int
lengthOfMinLength2List ( _, _, list ) =
    2 + List.length list


algorithms : PLL.Algorithms
algorithms =
    PLL.referenceAlgorithms


elementToString : { article : Article } -> PLL.RecognitionElement -> String
elementToString { article } element =
    let
        { indefiniteArticle, object } =
            case element of
                PLL.Pattern pattern ->
                    case pattern of
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

                        PLL.RightInsideTwoBar ->
                            { indefiniteArticle = Just "an", object = "inside two-bar on the right" }

                        PLL.LeftInsideTwoBar ->
                            { indefiniteArticle = Just "an", object = "inside two-bar on the left" }

                        _ ->
                            { indefiniteArticle = Nothing, object = "TODO" }

                PLL.Sticker sticker ->
                    case sticker of
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
    in
    case article of
        NoArticle ->
            object

        Indefinite ->
            (indefiniteArticle
                |> Maybe.map (\x -> x ++ " ")
                |> Maybe.withDefault ""
            )
                ++ object

        Definite ->
            "the " ++ object


isPlural : PLL.RecognitionElement -> Bool
isPlural element =
    case element of
        PLL.Sticker _ ->
            False

        PLL.Pattern pattern ->
            case pattern of
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

                PLL.RightInsideTwoBar ->
                    False

                PLL.LeftInsideTwoBar ->
                    False

                _ ->
                    False
