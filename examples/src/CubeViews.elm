module CubeViews exposing (main)

import Algorithm exposing (Algorithm)
import Cube exposing (Cube)
import Html exposing (..)
import Html.Attributes exposing (..)


cubeSize : Int
cubeSize =
    200


alg1 : Algorithm
alg1 =
    Algorithm.fromTurnList
        [ Algorithm.Turn Algorithm.M Algorithm.OneQuarter Algorithm.Clockwise
        , Algorithm.Turn Algorithm.E Algorithm.OneQuarter Algorithm.Clockwise
        , Algorithm.Turn Algorithm.S Algorithm.OneQuarter Algorithm.Clockwise
        ]


cubeToView1 : Cube
cubeToView1 =
    Cube.solved
        |> Cube.applyAlgorithm alg1


alg2 : Algorithm
alg2 =
    Algorithm.fromTurnList
        [ Algorithm.Turn Algorithm.R Algorithm.OneQuarter Algorithm.Clockwise
        , Algorithm.Turn Algorithm.U Algorithm.OneQuarter Algorithm.Clockwise
        ]


cubeToView2 : Cube
cubeToView2 =
    Cube.solved
        |> Cube.applyAlgorithm alg2


main : Html msg
main =
    div [ style "display" "flex", style "flex-wrap" "wrap", style "text-align" "center" ]
        [ -- ADD-STYLE-NODE-HERE-FOR-VISUAL-TESTING
          div []
            [ div [ style "height" (String.fromInt cubeSize ++ "px"), style "display" "flex", style "align-items" "center" ] [ text <| Algorithm.toString alg1 ]
            , div [ style "height" (String.fromInt cubeSize ++ "px"), style "display" "flex", style "align-items" "center" ] [ text <| Algorithm.toString alg2 ]
            ]
        , div []
            [ text "UFR"
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ufrDisplayAngle
                , annotateFaces = False

                -- ADD-THEME-HERE-IF-CONVERTING-TO-ADVANCED-VIEW
                }
                cubeToView1
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ufrDisplayAngle
                , annotateFaces = True

                -- ADD-THEME-HERE-IF-CONVERTING-TO-ADVANCED-VIEW
                }
                cubeToView2
            ]
        , div []
            [ text "UBL"
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ublDisplayAngle
                , annotateFaces = False

                -- ADD-THEME-HERE-IF-CONVERTING-TO-ADVANCED-VIEW
                }
                cubeToView1
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ublDisplayAngle
                , annotateFaces = True

                -- ADD-THEME-HERE-IF-CONVERTING-TO-ADVANCED-VIEW
                }
                cubeToView2
            ]
        , div []
            [ text "DBL"
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.dblDisplayAngle
                , annotateFaces = False

                -- ADD-THEME-HERE-IF-CONVERTING-TO-ADVANCED-VIEW
                }
                cubeToView1
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.dblDisplayAngle
                , annotateFaces = True

                -- ADD-THEME-HERE-IF-CONVERTING-TO-ADVANCED-VIEW
                }
                cubeToView2
            ]
        ]
