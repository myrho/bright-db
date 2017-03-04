module Bright.Uris exposing (..)

{-|
@docs isA, range, nilUri, defaultTypeUri
-}

import Graph exposing (Uri)


{-|
-}
isA : Uri
isA =
    "http://rdfs/isA"


{-|
-}
range : Uri
range =
    "http://rdfs/contains"


{-|
-}
nilUri : Uri
nilUri =
    "http://nil"


{-|
-}
defaultTypeUri : Uri
defaultTypeUri =
    "http://thing"
