module Expect.Extra exposing (equalCubeRenderings, equalListMembers, equalNonEmptyListMembers)

import Cube.Advanced
import Expect
import List.Nonempty
import TestHelpers.Cube


equalListMembers : List a -> List a -> Expect.Expectation
equalListMembers expected actual =
    let
        extraElements =
            List.filter (\x -> not <| List.member x expected) actual

        missingElements =
            List.filter (\x -> not <| List.member x actual) expected
    in
    if missingElements == [] && extraElements == [] then
        Expect.pass

    else
        Expect.fail <|
            "Lists don't have equal members\n"
                ++ "\n"
                ++ "The list had these extra elements:\n"
                ++ "\n"
                ++ Debug.toString extraElements
                ++ "\n\n"
                ++ "The list was missing these elements:\n"
                ++ "\n"
                ++ Debug.toString missingElements


equalNonEmptyListMembers : List.Nonempty.Nonempty a -> List.Nonempty.Nonempty a -> Expect.Expectation
equalNonEmptyListMembers expected actual =
    equalListMembers
        (List.Nonempty.toList expected)
        (List.Nonempty.toList actual)


equalCubeRenderings : Cube.Advanced.Rendering -> Cube.Advanced.Rendering -> Expect.Expectation
equalCubeRenderings expected actual =
    if expected == actual then
        Expect.pass

    else
        Expect.fail <|
            "The given cube rendering does not match the expected rendering"
                ++ "\n\n"
                ++ "(Actual != Expected)"
                ++ "\n\n"
                ++ TestHelpers.Cube.compareCubeRenderings actual expected
