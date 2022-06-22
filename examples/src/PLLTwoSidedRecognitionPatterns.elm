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
                        ([ div [ style "align-self" "center" ] [ text (PLL.getLetters pll ++ ": ") ]
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
                                                [ text <|
                                                    explainPLLRecognitionPattern <|
                                                        (Result.withDefault
                                                            { caseRecognition =
                                                                { patterns = Nothing
                                                                , absentPatterns = Nothing
                                                                , oppositelyColored = []
                                                                , notOppositelyColored = []
                                                                , adjacentlyColored = []
                                                                , identicallyColored = Nothing
                                                                , differentlyColored = Nothing
                                                                , noOtherStickersMatchThanThese = Nothing
                                                                , noOtherBlocksPresent = False
                                                                }
                                                            , postAUFRecognition =
                                                                List.Nonempty.singleton
                                                                    { elementsWithOriginalFace =
                                                                        List.Nonempty.singleton <|
                                                                            ( PLL.Pattern PLL.SixChecker
                                                                            , Cube.Advanced.UpOrDown Cube.Advanced.U
                                                                            )
                                                                    , finalFace = Cube.Advanced.UpOrDown Cube.Advanced.U
                                                                    }
                                                            }
                                                         <|
                                                            PLL.getUniqueTwoSidedRecognitionSpecification
                                                                jpermsAlgorithms
                                                                PLL.ufrRecognitionAngle
                                                                ( Debug.log "preAUF" preAUF, Debug.log "pll" pll )
                                                        )
                                                , Cube.view [ style "align-self" "center" ]
                                                    { pixelSize = 50
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
            Algorithm.fromString "(R U R' F') (R U R' U') R' F R2 U' R')"
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


log : Algorithm -> Algorithm
log x =
    let
        _ =
            Debug.log "algorithm" <| Algorithm.toString x
    in
    x


explainPLLRecognitionPattern : PLL.RecognitionSpecification -> String
explainPLLRecognitionPattern spec =
    let
        sortedSpec =
            sortForDisplay spec

        { patterns, absentPatterns, oppositelyColored, notOppositelyColored, adjacentlyColored, identicallyColored, differentlyColored, noOtherStickersMatchThanThese } =
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
                                }
                                (List.Nonempty.map PLL.Pattern patterns_)
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
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
                                { article = NoArticle
                                , finalConjunction = Or
                                , separator = separator
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
                            ++ nonemptyToSentenceList { article = Definite, finalConjunction = And, separator = separator } second
                    )
            , notOppositelyColored
                |> List.map
                    (\( first, second ) ->
                        nonemptyToSentenceList { article = Definite, finalConjunction = And, separator = separator } first
                            ++ " "
                            ++ (if
                                    (List.Nonempty.length first > 1)
                                        || isPlural (List.Nonempty.head first)
                                then
                                    "are"

                                else
                                    "is"
                               )
                            ++ " not the opposite color of "
                            ++ nonemptyToSentenceList
                                { article = Definite
                                , finalConjunction = And
                                , separator = separator
                                }
                                second
                    )
            , adjacentlyColored
                |> List.map
                    (\( first, second ) ->
                        nonemptyToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
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
                            ++ nonemptyToSentenceList
                                { article = Definite
                                , finalConjunction = And
                                , separator = separator
                                }
                                second
                    )
            , identicallyColored
                |> Maybe.map
                    (\elements ->
                        minLength2ToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
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
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , differentlyColored
                |> Maybe.map
                    (\elements ->
                        minLength2ToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
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
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            , noOtherStickersMatchThanThese
                |> Maybe.map
                    (\elements ->
                        "There are no other stickers that match the color of any other sticker than "
                            ++ nonemptyToSentenceList
                                { article = Definite
                                , finalConjunction = And
                                , separator = separator
                                }
                                elements
                    )
                |> Maybe.map List.singleton
                |> Maybe.withDefault []
            ]

        listOfPostAUFPatternsToLookAt =
            Debug.log "AUF" postAUFRecognition
                |> List.Nonempty.toList
                |> List.map
                    (\({ elementsWithOriginalFace, finalFace } as arg) ->
                        nonemptyToSentenceList
                            { article = Definite
                            , finalConjunction = And
                            , separator = separator
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
        , notOppositelyColored =
            caseRecognition.notOppositelyColored
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
            Maybe.map
                (sortMinLength2ListWith sortByFurthestLeftComparison)
                caseRecognition.identicallyColored
        , differentlyColored =
            Maybe.map
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


toSentenceList : { article : Article, finalConjunction : Conjunction, separator : Separator } -> List PLL.RecognitionElement -> String
toSentenceList { article, finalConjunction, separator } list =
    list
        |> List.map (elementToString { article = article })
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


nonemptyToSentenceList : { article : Article, finalConjunction : Conjunction, separator : Separator } -> List.Nonempty.Nonempty PLL.RecognitionElement -> String
nonemptyToSentenceList args =
    List.Nonempty.toList >> toSentenceList args


minLength2ToSentenceList : { article : Article, finalConjunction : Conjunction, separator : Separator } -> ( PLL.RecognitionElement, PLL.RecognitionElement, List PLL.RecognitionElement ) -> String
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
        -- trying to handle the cases manually and moving things between the first
        -- spots and the tail etc.
        _ ->
            ( first, second, tail )


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
