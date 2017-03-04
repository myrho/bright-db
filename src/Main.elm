module Main exposing (..)

import Bright.Model
import Bright.Update
import Bright.Sub


main =
    Platform.programWithFlags
        { init = Bright.Model.init
        , update = Bright.Update.update
        , subscriptions = Bright.Sub.subscriptions
        }
