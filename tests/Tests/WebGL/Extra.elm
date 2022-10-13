module Tests.WebGL.Extra exposing (suite)

import Expect
import Test exposing (..)
import WebGL
import WebGL.Extra


suite : Test
suite =
    describe "WebGL Extra"
        [ test "our hardcoded default options match the WebGL libraries default options" <|
            \_ ->
                matchesDefaultOptions WebGL.Extra.toHtmlDefaultOptions
                    |> Expect.equal True
                    |> Expect.onFail "output of toHtml with empty lists is the same as output of toHtmlWith with given options"
        , test "making sure it's not evergreen by asserting that an empty option list doesn't match the default options" <|
            \_ ->
                matchesDefaultOptions []
                    |> Expect.equal False
                    |> Expect.onFail "empty option list shouldn't match the default options"
        ]


matchesDefaultOptions : List WebGL.Option -> Bool
matchesDefaultOptions options =
    WebGL.toHtml [] [] == WebGL.toHtmlWith options [] []
