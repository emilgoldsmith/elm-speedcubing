module PLLTwoSidedRecognitionPatterns exposing (main)

import AUF
import Algorithm exposing (Algorithm)
import Cube
import Cube.Advanced
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
                    div
                        [ style "display" "flex"
                        , style "flex-direction" "row"
                        , style "margin" "40px"
                        , style "font-size" "10px"
                        ]
                        ([ div [ style "align-self" "center", style "margin-right" "7px" ] [ text (PLL.getLetters pll ++ ": ") ]
                         ]
                            ++ (AUF.all
                                    |> List.Nonempty.toList
                                    |> List.map
                                        (\preAUF ->
                                            div
                                                [ style "margin-right" "20px"
                                                , style "display" "flex"
                                                , style "flex-direction" "column"
                                                ]
                                                [ Result.withDefault (text "Error occurred") <|
                                                    Result.map (text << explainPLLRecognitionPattern) <|
                                                        PLL.getUniqueTwoSidedRecognitionSpecification
                                                            jpermsAlgorithms
                                                            PLL.ufrRecognitionAngle
                                                            ( preAUF, pll )
                                                , Cube.view [ style "align-self" "center" ]
                                                    { pixelSize = 50
                                                    , displayAngle = Cube.ufrDisplayAngle
                                                    , annotateFaces = True
                                                    }
                                                  <|
                                                    Cube.applyAlgorithm
                                                        (Algorithm.inverse <|
                                                            Algorithm.append
                                                                (Cube.makeAlgorithmMaintainOrientation <|
                                                                    Algorithm.append (AUF.toAlgorithm preAUF) <|
                                                                        PLL.getAlgorithm jpermsAlgorithms pll
                                                                )
                                                                (AUF.toAlgorithm preAUF)
                                                        )
                                                        Cube.solved
                                                ]
                                        )
                               )
                        )
                )
        )


explainPLLRecognitionPattern : PLL.RecognitionSpecification -> String
explainPLLRecognitionPattern spec =
    let
        sortedSpec =
            sortForDisplay spec

        { patterns, absentPatterns, oppositelyColored, adjacentlyColored, identicallyColored, differentlyColored, noOtherStickersMatchThanThese, noOtherBlocksPresent } =
            sortedSpec.caseRecognition

        { postAUFRecognition } =
            sortedSpec

        separator =
            Comma

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
                                { article = Definite
                                , finalConjunction = And
                                , separator = separator
                                , forcePlural = False
                                }
                                (List.Nonempty.map PLL.Pattern patterns_)
                            ++ (if noOtherBlocksPresent then
                                    ". No other blocks appear"

                                else
                                    ""
                               )
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault
                    (if noOtherBlocksPresent then
                        [ "No blocks appear at all" ]

                     else
                        []
                    )
            , absentPatterns
                |> Maybe.map
                    (\patterns_ ->
                        let
                            plural =
                                (List.Nonempty.length patterns_ > 1)
                                    || isPlural (PLL.Pattern <| List.Nonempty.head patterns_)
                        in
                        "There "
                            ++ (if plural then
                                    "are"

                                else
                                    "is"
                               )
                            ++ " no "
                            ++ nonemptyToSentenceList
                                { article = NoArticle
                                , finalConjunction = Or
                                , separator = separator
                                , forcePlural = plural
                                }
                                (List.Nonempty.map PLL.Pattern patterns_)
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , oppositelyColored
                |> List.map
                    (\( first, second ) ->
                        nonemptyToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
                            , forcePlural = False
                            }
                            first
                            ++ " "
                            ++ (if
                                    (List.Nonempty.length first > 1)
                                        || isPlural (List.Nonempty.head first)
                                then
                                    "are"

                                else
                                    "is"
                               )
                            ++ " the opposite color of "
                            ++ (if
                                    ((first == List.Nonempty.singleton (PLL.Pattern PLL.LeftHeadlights))
                                        && (second == List.Nonempty.singleton (PLL.Sticker PLL.SecondStickerFromLeft))
                                    )
                                        || ((first == List.Nonempty.singleton (PLL.Pattern PLL.RightHeadlights))
                                                && (second == List.Nonempty.singleton (PLL.Sticker PLL.SecondStickerFromRight))
                                           )
                                then
                                    "the enclosed sticker"

                                else
                                    nonemptyToSentenceList { article = Definite, finalConjunction = And, separator = separator, forcePlural = False } second
                               )
                    )
            , adjacentlyColored
                |> List.map
                    (\( first, second ) ->
                        nonemptyToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
                            , forcePlural = False
                            }
                            first
                            ++ " "
                            ++ (if
                                    (List.Nonempty.length first > 1)
                                        || isPlural (List.Nonempty.head first)
                                then
                                    "are"

                                else
                                    "is"
                               )
                            ++ " the adjacent color of "
                            ++ (if
                                    ((first == List.Nonempty.singleton (PLL.Pattern PLL.LeftHeadlights))
                                        && (second == List.Nonempty.singleton (PLL.Sticker PLL.SecondStickerFromLeft))
                                    )
                                        || ((first == List.Nonempty.singleton (PLL.Pattern PLL.RightHeadlights))
                                                && (second == List.Nonempty.singleton (PLL.Sticker PLL.SecondStickerFromRight))
                                           )
                                then
                                    "the enclosed sticker"

                                else
                                    nonemptyToSentenceList { article = Definite, finalConjunction = And, separator = separator, forcePlural = False } second
                               )
                    )
            , identicallyColored
                |> List.map
                    (\elements ->
                        minLength2ToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
                            , forcePlural = False
                            }
                            elements
                            ++ " are "
                            ++ (if lengthOfMinLength2List elements > 2 then
                                    "all "

                                else
                                    ""
                               )
                            ++ "the same color"
                    )
            , differentlyColored
                |> List.map
                    (\elements ->
                        minLength2ToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
                            , forcePlural = False
                            }
                            elements
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
                        "There are no other stickers that match the color of any other sticker except for "
                            ++ nonemptyToSentenceList
                                { article = Definite
                                , finalConjunction = And
                                , separator = separator
                                , forcePlural = False
                                }
                                elements
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            ]

        listOfPostAUFPatternsToLookAt =
            postAUFRecognition
                |> List.Nonempty.toList
                |> List.map
                    (\({ elementsWithOriginalFace, finalFace } as arg) ->
                        nonemptyToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
                            , forcePlural = False
                            }
                            (List.Nonempty.map Tuple.first elementsWithOriginalFace)
                            ++ (if staysInSamePlace arg then
                                    " which will stay in place"

                                else
                                    " which will end up on the "
                                        ++ (case finalFace of
                                                Cube.Advanced.UpOrDown Cube.Advanced.U ->
                                                    "top"

                                                Cube.Advanced.UpOrDown Cube.Advanced.D ->
                                                    "bottom"

                                                Cube.Advanced.LeftOrRight Cube.Advanced.L ->
                                                    "left"

                                                Cube.Advanced.LeftOrRight Cube.Advanced.R ->
                                                    "right"

                                                Cube.Advanced.FrontOrBack Cube.Advanced.F ->
                                                    "front"

                                                Cube.Advanced.FrontOrBack Cube.Advanced.B ->
                                                    "back"
                                           )
                                        ++ " side of the cube"
                               )
                    )
    in
    (parts
        |> List.concat
        |> List.map capitalize
        |> String.join ". "
        |> String.trim
    )
        ++ ". You can identify what the last AUF will be by looking at "
        ++ buildSentenceList
            { finalConjunction = Or, separator = Semicolon }
            listOfPostAUFPatternsToLookAt
        ++ "."


staysInSamePlace :
    { elementsWithOriginalFace : List.Nonempty.Nonempty ( PLL.RecognitionElement, Cube.Advanced.Face )
    , finalFace : Cube.Advanced.Face
    }
    -> Bool
staysInSamePlace { elementsWithOriginalFace, finalFace } =
    elementsWithOriginalFace
        |> List.Nonempty.all
            (Tuple.second >> (==) finalFace)


sortForDisplay : PLL.RecognitionSpecification -> PLL.RecognitionSpecification
sortForDisplay { caseRecognition, postAUFRecognition } =
    { caseRecognition =
        { patterns =
            Maybe.map
                (List.Nonempty.sortWith sortPatternsByFurthestLeftComparison)
                caseRecognition.patterns
        , absentPatterns =
            Maybe.map
                (List.Nonempty.sortWith sortPatternsByFurthestLeftComparison)
                caseRecognition.absentPatterns
        , oppositelyColored =
            caseRecognition.oppositelyColored
                |> List.map ensurePatternsAreInFirstSpot
                |> List.map
                    (Tuple.mapBoth
                        (List.Nonempty.sortWith sortByFurthestLeftComparison)
                        (List.Nonempty.sortWith sortByFurthestLeftComparison)
                    )
        , adjacentlyColored =
            caseRecognition.adjacentlyColored
                |> List.map ensurePatternsAreInFirstSpot
                |> List.map
                    (Tuple.mapBoth
                        (List.Nonempty.sortWith sortByFurthestLeftComparison)
                        (List.Nonempty.sortWith sortByFurthestLeftComparison)
                    )
        , identicallyColored =
            List.map
                (sortMinLength2ListWith sortByFurthestLeftComparison)
                caseRecognition.identicallyColored
        , differentlyColored =
            List.map
                (sortMinLength2ListWith sortByFurthestLeftComparison)
                caseRecognition.differentlyColored
        , noOtherStickersMatchThanThese =
            Maybe.map
                (List.Nonempty.sortWith sortByFurthestLeftComparison)
                caseRecognition.noOtherStickersMatchThanThese
        , noOtherBlocksPresent = caseRecognition.noOtherBlocksPresent
        }
    , postAUFRecognition =
        List.Nonempty.map
            (\arg ->
                { arg
                    | elementsWithOriginalFace =
                        List.Nonempty.sortWith
                            sortTupleByFurthestLeftComparison
                            arg.elementsWithOriginalFace
                }
            )
            postAUFRecognition
    }


sortTupleByFurthestLeftComparison : ( PLL.RecognitionElement, a ) -> ( PLL.RecognitionElement, a ) -> Order
sortTupleByFurthestLeftComparison ( a, _ ) ( b, _ ) =
    sortByFurthestLeftComparison a b


sortByFurthestLeftComparison : PLL.RecognitionElement -> PLL.RecognitionElement -> Order
sortByFurthestLeftComparison a b =
    let
        toFloat element =
            case element of
                PLL.Sticker sticker ->
                    case sticker of
                        PLL.FirstStickerFromLeft ->
                            1

                        PLL.SecondStickerFromLeft ->
                            2

                        PLL.ThirdStickerFromLeft ->
                            3

                        PLL.ThirdStickerFromRight ->
                            4

                        PLL.SecondStickerFromRight ->
                            5

                        PLL.FirstStickerFromRight ->
                            6

                PLL.Pattern pattern ->
                    case pattern of
                        PLL.Bookends ->
                            1.5

                        PLL.LeftHeadlights ->
                            1

                        PLL.RightHeadlights ->
                            4

                        PLL.RightOutsideTwoBar ->
                            5

                        PLL.LeftOutsideTwoBar ->
                            1

                        PLL.RightInsideTwoBar ->
                            4

                        PLL.LeftInsideTwoBar ->
                            2

                        PLL.LeftThreeBar ->
                            1

                        PLL.RightThreeBar ->
                            4

                        PLL.LeftFourChecker ->
                            1

                        PLL.RightFourChecker ->
                            3

                        PLL.InnerFourChecker ->
                            2

                        PLL.LeftFiveChecker ->
                            1

                        PLL.RightFiveChecker ->
                            2

                        PLL.SixChecker ->
                            1
    in
    compare (toFloat a) (toFloat b)


sortPatternsByFurthestLeftComparison : PLL.RecognitionPattern -> PLL.RecognitionPattern -> Order
sortPatternsByFurthestLeftComparison a b =
    sortByFurthestLeftComparison (PLL.Pattern a) (PLL.Pattern b)


ensurePatternsAreInFirstSpot : ( List.Nonempty.Nonempty PLL.RecognitionElement, List.Nonempty.Nonempty PLL.RecognitionElement ) -> ( List.Nonempty.Nonempty PLL.RecognitionElement, List.Nonempty.Nonempty PLL.RecognitionElement )
ensurePatternsAreInFirstSpot ( a, b ) =
    if
        b
            |> List.Nonempty.toList
            |> List.filter
                (\element ->
                    case element of
                        PLL.Pattern _ ->
                            True

                        PLL.Sticker _ ->
                            False
                )
            |> List.isEmpty
    then
        ( a, b )

    else
        ( b, a )


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


type Separator
    = Comma
    | Semicolon


toSentenceList : { article : Article, finalConjunction : Conjunction, separator : Separator, forcePlural : Bool } -> List PLL.RecognitionElement -> String
toSentenceList { article, finalConjunction, separator, forcePlural } list =
    list
        |> List.map (elementToString { article = article, forcePlural = forcePlural })
        |> buildSentenceList { finalConjunction = finalConjunction, separator = separator }


buildSentenceList : { finalConjunction : Conjunction, separator : Separator } -> List String -> String
buildSentenceList { finalConjunction, separator } strings =
    let
        separatorString =
            case separator of
                Comma ->
                    ","

                Semicolon ->
                    ";"

        conjunctionString =
            case finalConjunction of
                And ->
                    "and"

                Or ->
                    "or"
    in
    case strings of
        [] ->
            ""

        [ x ] ->
            x

        [ x, y ] ->
            x ++ separatorString ++ " " ++ conjunctionString ++ " " ++ y

        x :: xs ->
            x ++ separatorString ++ " " ++ buildSentenceList { finalConjunction = finalConjunction, separator = separator } xs


nonemptyToSentenceList : { article : Article, finalConjunction : Conjunction, separator : Separator, forcePlural : Bool } -> List.Nonempty.Nonempty PLL.RecognitionElement -> String
nonemptyToSentenceList args =
    List.Nonempty.toList >> toSentenceList args


minLength2ToSentenceList : { article : Article, finalConjunction : Conjunction, separator : Separator, forcePlural : Bool } -> ( PLL.RecognitionElement, PLL.RecognitionElement, List PLL.RecognitionElement ) -> String
minLength2ToSentenceList args ( first, second, rest ) =
    toSentenceList args (first :: second :: rest)


lengthOfMinLength2List : ( a, a, List a ) -> Int
lengthOfMinLength2List ( _, _, list ) =
    2 + List.length list


sortMinLength2ListWith : (a -> a -> Order) -> ( a, a, List a ) -> ( a, a, List a )
sortMinLength2ListWith comp ( first, second, tail ) =
    let
        allSorted =
            List.sortWith comp (first :: second :: tail)
    in
    case allSorted of
        x1 :: x2 :: xs ->
            ( x1, x2, xs )

        -- This will obviously never happen as we just created the list above with 2
        -- elements in it at the least. Just for the types and simpler code than
        -- trying to sort more manually and moving things between the first
        -- spots and the tail etc.
        _ ->
            ( first, second, tail )


elementToString : { article : Article, forcePlural : Bool } -> PLL.RecognitionElement -> String
elementToString { article, forcePlural } element =
    let
        { indefiniteArticle, object, pluralized } =
            case element of
                PLL.Pattern pattern ->
                    case pattern of
                        PLL.Bookends ->
                            { indefiniteArticle = Nothing, object = "bookends", pluralized = "bookends" }

                        PLL.LeftHeadlights ->
                            { indefiniteArticle = Nothing, object = "headlights on the left", pluralized = "headlights on the left" }

                        PLL.RightHeadlights ->
                            { indefiniteArticle = Nothing, object = "headlights on the right", pluralized = "headlights on the right" }

                        PLL.RightOutsideTwoBar ->
                            { indefiniteArticle = Just "an", object = "outside two-bar on the right", pluralized = "outside two-bars on the right" }

                        PLL.LeftOutsideTwoBar ->
                            { indefiniteArticle = Just "an", object = "outside two-bar on the left", pluralized = "outside two-bars on the left" }

                        PLL.RightInsideTwoBar ->
                            { indefiniteArticle = Just "an", object = "inside two-bar on the right", pluralized = "inside two-bars on the right" }

                        PLL.LeftInsideTwoBar ->
                            { indefiniteArticle = Just "an", object = "inside two-bar on the left", pluralized = "inside two-bars on the left" }

                        PLL.LeftThreeBar ->
                            { indefiniteArticle = Just "a", object = "three-bar on the left", pluralized = "three-bars on the left" }

                        PLL.RightThreeBar ->
                            { indefiniteArticle = Just "a", object = "three-bar on the right", pluralized = "three-bars on the right" }

                        PLL.LeftFourChecker ->
                            { indefiniteArticle = Just "a", object = "four checker pattern on the left", pluralized = "four checker patterns on the left" }

                        PLL.RightFourChecker ->
                            { indefiniteArticle = Just "a", object = "four checker pattern on the right", pluralized = "four checker patterns on the right" }

                        PLL.InnerFourChecker ->
                            { indefiniteArticle = Just "an", object = "inside four checker pattern", pluralized = "inside four checker patterns" }

                        PLL.LeftFiveChecker ->
                            { indefiniteArticle = Just "a", object = "five checker pattern on the left", pluralized = "five checker patterns on the left" }

                        PLL.RightFiveChecker ->
                            { indefiniteArticle = Just "a", object = "five checker pattern on the right", pluralized = "five checker patterns on the right" }

                        PLL.SixChecker ->
                            { indefiniteArticle = Just "a", object = "six checker pattern", pluralized = "six checker patterns" }

                PLL.Sticker sticker ->
                    case sticker of
                        PLL.FirstStickerFromLeft ->
                            { indefiniteArticle = Nothing, object = "first sticker from the left", pluralized = "first stickers from the left" }

                        PLL.FirstStickerFromRight ->
                            { indefiniteArticle = Nothing, object = "first sticker from the right", pluralized = "first stickers from the right" }

                        PLL.SecondStickerFromLeft ->
                            { indefiniteArticle = Nothing, object = "second sticker from the left", pluralized = "second stickers from the left" }

                        PLL.SecondStickerFromRight ->
                            { indefiniteArticle = Nothing, object = "second sticker from the right", pluralized = "second stickers from the right" }

                        PLL.ThirdStickerFromLeft ->
                            { indefiniteArticle = Nothing, object = "third sticker from the left", pluralized = "third stickers from the left" }

                        PLL.ThirdStickerFromRight ->
                            { indefiniteArticle = Nothing, object = "third sticker from the right", pluralized = "third stickers from the right" }

        pluralHandledObject =
            if forcePlural then
                pluralized

            else
                object
    in
    case article of
        NoArticle ->
            pluralHandledObject

        Indefinite ->
            (indefiniteArticle
                |> Maybe.map (\x -> x ++ " ")
                |> Maybe.withDefault ""
            )
                ++ pluralHandledObject

        Definite ->
            "the " ++ pluralHandledObject


isPlural : PLL.RecognitionElement -> Bool
isPlural element =
    case element of
        PLL.Sticker _ ->
            False

        PLL.Pattern pattern ->
            case pattern of
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


jpermsAlgorithms : PLL.Algorithms
jpermsAlgorithms =
    { ua =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "M2 U M U2 M' U M2"
    , ub =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "M2 U' M U2 M' U' M2"
    , h =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "M2 U M2 U2 M2 U M2"
    , z =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "M U M2 U M2 U M U2 M2"
    , aa =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "x L2 D2 (L' U' L) D2 (L' U L')"
    , ab =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "x (L U' L) D2 (L' U L) D2 L2"
    , e =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "x' (L' U L D') (L' U' L D) (L' U' L D') (L' U L D)"
    , t =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U R' U') R' F R2 U' R' U' (R U R') F'"
    , f =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R' U' F' (R U R' U') R' F R2 U' R' U' (R U R') U R"
    , jb =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U R' F') (R U R' U') R' F R2 U' R'"
    , ja =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "x (R2 F R F') R U2 (r' U r) U2"
    , ra =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U' R' U') (R U R D) (R' U' R D') (R' U2 R')"
    , rb =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R2 F R (U R U' R') F' R U2 R' U2 R"
    , y =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "F (R U' R' U') (R U R') F' (R U R' U') (R' F R F')"
    , v =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R U' (R U R') D R D' R (U' D) R2 U R2 D' R2"
    , na =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U R' U) (R U R' F' R U R' U' R' F R2 U' R') (U2 R U' R')"
    , nb =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "r' D' F (r U' r') F' D (r2 U r' U') (r' F r F')"
    , ga =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R2 U R' U R' U' R U' R2 (U' D) (R' U R) D'"
    , gb =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R' U' R) (U D') R2 U R' U R U' R U' R2 D"
    , gc =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "R2 U' R U' R U R' U R2 (U D') (R U' R') D"
    , gd =
        Result.withDefault Algorithm.empty <|
            Algorithm.fromString "(R U R') (U' D) R2 U' R U' R' U R' U R2 D'"
    }
