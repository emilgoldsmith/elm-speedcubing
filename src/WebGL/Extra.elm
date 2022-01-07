module WebGL.Extra exposing (toHtmlDefaultOptions)

import WebGL


toHtmlDefaultOptions : List WebGL.Option
toHtmlDefaultOptions =
    [ WebGL.alpha True, WebGL.antialias, WebGL.depth 1 ]
