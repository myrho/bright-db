module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Test.RemoteUpdate
import Test.Bright


all : Test
all =
    describe "All"
        [ Test.RemoteUpdate.all
        , Test.Bright.all
        ]
