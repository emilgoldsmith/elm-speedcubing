module CubeViews exposing (main)

import Cube
import Html exposing (..)
import Html.Attributes exposing (..)


cubeSize = 400

main =
    div []
        [ div [ style "display" "flex" ]
            [ Cube.view []
                { pixelSize = cubeSize
                , cube = Cube.solved
                , displayAngle = Cube.ufrDisplayAngle
                , annotateFaces = False
                }
            , Cube.view []
                { pixelSize = cubeSize
                , cube = Cube.solved
                , displayAngle = Cube.ufrDisplayAngle
                , annotateFaces = True
                }
            ]
        , div [ style "display" "flex" ]
            [ Cube.view []
                { pixelSize = cubeSize
                , cube = Cube.solved
                , displayAngle = Cube.ublDisplayAngle
                , annotateFaces = False
                }
            , Cube.view []
                { pixelSize = cubeSize
                , cube = Cube.solved
                , displayAngle = Cube.ublDisplayAngle
                , annotateFaces = True
                }
            ]
        ]
