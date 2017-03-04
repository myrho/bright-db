module Bright.Sub exposing (subscriptions)

import Json.Decode as Dec exposing (Decoder)
import Bright.Model exposing (..)
import Bright.Decoder exposing (..)
import Bright.Ports
import Bright.Update exposing (..)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receive
        , appReceive
        ]


receive : Sub Msg
receive =
    Bright.Ports.receive
        (Dec.decodeValue decodeReceive
            >> Result.withDefault (Error "could not decode data")
        )


appReceive : Sub Msg
appReceive =
    Bright.Ports.appReceive
        (Dec.decodeValue decodeAppReceive
            >> Debug.log "result"
            >> Result.withDefault (Error "could not decode appReceive")
        )
