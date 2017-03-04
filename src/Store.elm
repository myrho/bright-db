module Store exposing (..)

import Dict exposing (Dict)
import Maybe.Extra exposing (join)
import Json.Encode as Enc
import Json.Decode as Dec exposing (Decoder)
import Tuple exposing (mapSecond)


type Store comparable comparable o
    = Store (Dict comparable (Dict comparable o))


empty : Store comparable comparable o
empty =
    Store Dict.empty


get : comparable -> Store comparable comparable o -> Maybe (Dict comparable o)
get s (Store st) =
    Dict.get s st


getObject : comparable -> comparable -> Store comparable comparable o -> Maybe o
getObject s p (Store st) =
    Dict.get s st
        |> Maybe.map (Dict.get p)
        |> Maybe.Extra.join


insertObject : comparable -> comparable -> o -> Store comparable comparable o -> Store comparable comparable o
insertObject s p o (Store st) =
    Dict.get s st
        |> Maybe.withDefault Dict.empty
        |> Dict.insert p o
        |> (\d -> Dict.insert s d st)
        |> Store


toDicts : Store comparable comparable o -> Dict comparable (Dict comparable o)
toDicts (Store st) =
    st


encode : (comparable -> Enc.Value) -> (comparable -> Enc.Value) -> (o -> Enc.Value) -> Store comparable comparable o -> Enc.Value
encode encodeS encodeP encodeO (Store st) =
    Dict.toList st
        |> List.map
            (\( s, p ) ->
                Enc.list
                    [ encodeS s
                    , Dict.toList p
                        |> List.map
                            (\( p, o ) ->
                                Enc.list
                                    [ encodeP p
                                    , encodeO o
                                    ]
                            )
                        |> Enc.list
                    ]
            )
        |> Enc.list


decode : Decoder comparable -> Decoder comparable -> Decoder o -> Decoder (Store comparable comparable o)
decode decodeS decodeP decodeO =
    Dec.map (Store << Dict.fromList) <|
        Dec.list <|
            Dec.map2 (,)
                (Dec.index 0 decodeS)
                (Dec.index 1 <|
                    Dec.map Dict.fromList <|
                        Dec.list <|
                            Dec.map2 (,)
                                (Dec.index 0 decodeP)
                                (Dec.index 1 decodeO)
                )


map : (comparable -> comparable -> o -> u) -> Store comparable comparable o -> Store comparable comparable u
map func (Store st) =
    Dict.map (\s p -> Dict.map (func s) p) st
        |> Store


map2 : (comparable -> comparable -> o1 -> o2 -> u) -> Store comparable comparable o1 -> Store comparable comparable o2 -> Store comparable comparable u
map2 func st1 st2 =
    foldl
        (\s p o1 u ->
            case getObject s p st2 of
                Nothing ->
                    u

                Just o2 ->
                    func s p o1 o2
                        |> (flip (insertObject s p)) u
        )
        empty
        st1


isEmpty : Store comparable comparable o -> Bool
isEmpty (Store st) =
    Dict.isEmpty st


foldl : (comparable -> comparable -> o -> u -> u) -> u -> Store comparable comparable o -> u
foldl func acc (Store st) =
    st
        |> Dict.foldl
            (\s e acc ->
                Dict.foldl (func s) acc e
            )
            acc


fromList : List ( comparable, List ( comparable, o ) ) -> Store comparable comparable o
fromList =
    List.map (mapSecond Dict.fromList)
        >> Dict.fromList
        >> Store


union : Store comparable comparable o -> Store comparable comparable o -> Store comparable comparable o
union st1 st2 =
    foldl insertObject st2 st1


fromDicts : Dict comparable (Dict comparable o) -> Store comparable comparable o
fromDicts =
    Store


filter : (comparable -> Dict comparable o -> Bool) -> Store comparable comparable o -> Store comparable comparable o
filter func (Store st) =
    Dict.filter func st
        |> Store


update : comparable -> comparable -> (Maybe o -> Maybe o) -> Store comparable comparable o -> Store comparable comparable o
update s p upd st =
    getObject s p st
        |> upd
        |> Maybe.map (\o -> insertObject s p o st)
        |> Maybe.withDefault st


values : Store comparable comparable o -> List o
values (Store st) =
    Dict.values st
        |> List.map Dict.values
        |> List.concat
