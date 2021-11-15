module CubeViews exposing (main)

import Algorithm
import Cube exposing (Cube)
import Html exposing (..)
import Html.Attributes exposing (..)


cubeSize : Int
cubeSize =
    400


cubeToView1 : Cube
cubeToView1 =
    Cube.solved
        |> Cube.applyAlgorithm
            (Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.M Algorithm.Halfway Algorithm.Clockwise
                , Algorithm.Turn Algorithm.E Algorithm.Halfway Algorithm.Clockwise
                , Algorithm.Turn Algorithm.S Algorithm.Halfway Algorithm.Clockwise
                ]
            )

cubeToView2 : Cube
cubeToView2 =
    Cube.solved
        |> Cube.applyAlgorithm
            (Algorithm.fromTurnList
                [ Algorithm.Turn Algorithm.R Algorithm.Halfway Algorithm.Clockwise
                , Algorithm.Turn Algorithm.L Algorithm.Halfway Algorithm.Clockwise
                ]
            )


main : Html msg
main =
    div [ style "display" "flex", style "flex-wrap" "wrap" ]
        [ div [ style "display" "flex" ]
            [ Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ufrDisplayAngle
                , annotateFaces = False
                }
                cubeToView1
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ufrDisplayAngle
                , annotateFaces = True
                }
                cubeToView2
            ]
        , div [ style "display" "flex" ]
            [ Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ublDisplayAngle
                , annotateFaces = False
                }
                cubeToView1
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ublDisplayAngle
                , annotateFaces = True
                }
                cubeToView2
            ]
        , div [ style "display" "flex" ]
            [ Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.dblDisplayAngle
                , annotateFaces = False
                }
                cubeToView1
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.dblDisplayAngle
                , annotateFaces = True
                }
                cubeToView2
            ]
        ]
