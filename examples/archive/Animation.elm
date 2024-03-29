module Animation exposing (main)

import Algorithm exposing (Algorithm)
import Browser
import Cube exposing (Cube)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Nonempty
import PLL


main : Program () Model Msg
main =
    Browser.element
        { init = always init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- INIT


type alias Model =
    { cube : Cube
    , animationState : Cube.AnimationState
    , done : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { cube = Cube.solved
      , animationState = Cube.noAnimation
      , done = False
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = AnimationMsg Cube.AnimationMsg
    | StartAnimationFromScratch
    | PauseAnimation
    | UnpauseAnimation
    | AnimationDone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AnimationMsg animationMsg ->
            Cube.handleAnimationMsg
                { toMsg = AnimationMsg
                , animationDoneMsg = AnimationDone
                }
                model.animationState
                animationMsg
                |> Tuple.mapFirst (\x -> { model | animationState = x })

        StartAnimationFromScratch ->
            ( { model
                | animationState =
                    Cube.animateAlgorithm <|
                        Algorithm.fromTurnList (List.Nonempty.toList Algorithm.allTurns)
                , done = False
              }
            , Cmd.none
            )

        PauseAnimation ->
            ( { model
                | animationState = Cube.pauseAnimation model.animationState
              }
            , Cmd.none
            )

        UnpauseAnimation ->
            ( { model
                | animationState = Cube.unpauseAnimation model.animationState
              }
            , Cmd.none
            )

        AnimationDone ->
            ( { model
                | done = True
              }
            , Cmd.none
            )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "display" "flex"
        , style "width" "100%"
        , style "height" "100vh"
        , style "justify-content" "center"
        , style "align-items" "center"
        , style "flex-direction" "column"
        ]
        [ if model.done then
            div [ style "font-size" "30px" ] [ text "DONE" ]

          else
            div [] []
        , model.animationState
            |> Cube.currentTurnAnimating
            |> Maybe.map (List.singleton >> Algorithm.fromTurnList)
            |> Maybe.map (Algorithm.toString >> text)
            |> Maybe.withDefault (text "")
            |> (List.singleton
                    >> div [ style "height" "30px", style "font-size" "30px" ]
               )
        , Cube.viewAnimatable
            []
            { animationState = model.animationState
            , toMsg = AnimationMsg
            , pixelSize = 500
            , annotateFaces = False
            , displayAngle = Cube.ufrDisplayAngle
            }
            model.cube
        , button [ onClick StartAnimationFromScratch ] [ text "Start Animation From Scratch" ]
        , button [ onClick PauseAnimation ] [ text "Pause Animation" ]
        , button [ onClick UnpauseAnimation ] [ text "Unpause Animation" ]
        ]
