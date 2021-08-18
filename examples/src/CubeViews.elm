module CubeViews exposing (main)

import Cube
import Html exposing (..)
import Html.Attributes exposing (..)


cubeSize : Int
cubeSize =
    400


main : Html msg
main =
    div []
        [ div [ style "display" "flex" ]
            [ Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ufrDisplayAngle
                , annotateFaces = False
                }
                Cube.solved
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ufrDisplayAngle
                , annotateFaces = True
                }
                Cube.solved
            ]
        , div [ style "display" "flex" ]
            [ Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ublDisplayAngle
                , annotateFaces = False
                }
                Cube.solved
            , Cube.view []
                { pixelSize = cubeSize
                , displayAngle = Cube.ublDisplayAngle
                , annotateFaces = True
                }
                Cube.solved
            ]
        ]
