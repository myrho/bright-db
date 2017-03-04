module Bright.IO exposing (..)

import Json.Decode as Dec
import Json.Encode as Enc
import Dict
import Bright.Ports
import Bright.Model exposing (..)
import Bright.Decoder exposing (..)
import Bright.Encoder exposing (..)
import Graph exposing (Subject, Predicate, Uri)
import Bright.DB exposing (wildcard, Query, Entities, LocalOperations)
import Store


load : (Entities -> msg) -> Sub msg
load tag =
    Bright.Ports.load
        (Dec.decodeValue decodeEntities
            >> Result.withDefault Store.empty
            >> tag
        )


save : LocalOperations -> Cmd msg
save =
    encodeLocalOperations >> Debug.log "save" >> Bright.Ports.save


query : Query -> Cmd msg
query =
    encodeQuery_ >> Bright.Ports.localQuery
