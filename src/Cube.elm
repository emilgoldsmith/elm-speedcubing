module Cube exposing (Cube, applyAlgorithm, solved, viewUBLWithLetters, viewUFRNoLetters, viewUFRWithLetters, algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation)

{-| Documentation to come

@docs Cube, applyAlgorithm, solved, viewUBLWithLetters, viewUFRNoLetters, viewUFRWithLetters, algorithmResultsAreEquivalent, algorithmResultsAreEquivalentIndependentOfFinalRotation

-}

import Algorithm exposing (Algorithm)
import Html
import Internal.Cube
import List.Nonempty


{-| Placeholder
-}
type alias Cube =
    Internal.Cube.Cube


{-| Placeholder
-}
applyAlgorithm : Algorithm -> Cube -> Cube
applyAlgorithm =
    Internal.Cube.applyAlgorithm


{-| Placeholder
-}
solved : Cube
solved =
    Internal.Cube.solved


{-| Placeholder
-}
viewUBLWithLetters : List (Html.Attribute msg) -> Int -> Cube -> Html.Html msg
viewUBLWithLetters =
    Internal.Cube.viewUBLWithLetters


{-| Placeholder
-}
viewUFRNoLetters : List (Html.Attribute msg) -> Int -> Cube -> Html.Html msg
viewUFRNoLetters =
    Internal.Cube.viewUFRNoLetters


{-| Placeholder
-}
viewUFRWithLetters : List (Html.Attribute msg) -> Int -> Cube -> Html.Html msg
viewUFRWithLetters =
    Internal.Cube.viewUFRWithLetters


{-| Placeholder
-}
algorithmResultsAreEquivalent : Algorithm -> Algorithm -> Bool
algorithmResultsAreEquivalent a b =
    solved
        |> applyAlgorithm a
        |> applyAlgorithm (Algorithm.inverse b)
        |> (==) solved


{-| Placeholder
-}
algorithmResultsAreEquivalentIndependentOfFinalRotation : Algorithm -> Algorithm -> Bool
algorithmResultsAreEquivalentIndependentOfFinalRotation a b =
    List.Nonempty.map (Algorithm.append a) Algorithm.allCubeAngles
        |> List.Nonempty.any (algorithmResultsAreEquivalent b)
