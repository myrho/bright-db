module Bright.Util exposing (..)

import String
import Set
import Dict
import Array.Hamt as Array exposing (Array)
import Bright.Uris
import Bright.Model exposing (Model)
import Graph exposing (Uri, Subject, Predicate, Local)
import Store


isSubscribed : Model -> Uri -> Subject -> Predicate -> Bool
isSubscribed model peer subject predicate =
    Store.getObject subject predicate model.subscribed
        |> Maybe.map (Dict.member peer)
        |> Maybe.withDefault False


isUri : String -> Bool
isUri =
    String.startsWith "http"
