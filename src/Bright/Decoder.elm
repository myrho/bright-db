module Bright.Decoder exposing (..)

import Dict exposing (Dict)
import Json.Decode as Dec exposing (Decoder)
import Bright.Model exposing (..)
import Bright.Update exposing (Msg(..))
import Bright.DB exposing (Query, Entities, Object(..), RemoteOperations, LocalOperations, CharOrRefOps(..), CharOrRefRemoteOps(..), Meta, States)
import Graph exposing (Subject, Predicate, Uri, State, Local, Remote)
import LSEQ
import LSEQ.Types as LSEQ
import Store
import Set


(:=) =
    Dec.field


decodeUri : Decoder Uri
decodeUri =
    Dec.string


decodeAppReceive : Decoder Msg
decodeAppReceive =
    Dec.index 0 decodeUri
        |> Dec.andThen
            (\app ->
                Dec.index 1 <|
                    Dec.oneOf
                        [ decodeQuery |> Dec.map (LocalQuery app)
                        , decodeLocalOperations |> Dec.map (Save app)
                        ]
            )


decodeRemoteQuery : Peer -> Decoder Msg
decodeRemoteQuery peer =
    Dec.oneOf
        [ Dec.field "q" decodeQuery
            |> Dec.map (RemoteQuery peer)
        , Dec.field "r" decodeQuery
            |> Dec.map (ReQuery peer)
        ]


decodeQuery : Decoder Query
decodeQuery =
    Dec.map3 (,,)
        (Dec.index 0 Dec.string)
        (Dec.index 1 Dec.string)
        (Dec.index 2 Dec.string)


{-|
Decodes data coming from other peers
-}
decodeReceive : Decoder Msg
decodeReceive =
    Dec.index 0 decodeUri
        |> Dec.andThen
            (\peer ->
                Dec.index 1 <|
                    Dec.oneOf
                        [ decodeRemoteOperations
                            |> Dec.map (Load peer)
                        , decodeStates
                            |> Dec.map (Subscriber peer)
                        , decodeRemoteQuery peer
                        , decodeMeta
                            |> Dec.map (ReceiveMeta peer)
                        , Dec.bool
                            |> Dec.andThen
                                (\online ->
                                    if online then
                                        Dec.succeed (AddPeer peer)
                                    else
                                        Dec.succeed (ClosePeer peer)
                                )
                        ]
            )


decodeMeta : Decoder Meta
decodeMeta =
    Dec.map2 (,)
        (Dec.index 0 decodeUri)
        (Dec.index 1 <| Dec.map Set.fromList <| Dec.list decodeUri)
        |> Dec.list
        |> Dec.map Dict.fromList


{-|
Decode localOperations from an app to be saved.
-}
decodeAppSave : Decoder ( Uri, LocalOperations )
decodeAppSave =
    Dec.map2 (,)
        (Dec.index 0 decodeUri)
        (Dec.index 1 decodeLocalOperations)


{-|
Decode the uri of an app, to be registered.
-}
decodeApp : Decoder Uri
decodeApp =
    decodeUri


decodeSubscribed : Decoder Key
decodeSubscribed =
    Dec.field "subscribed" decodeKey


decodeIndexSubscribed : Decoder Key
decodeIndexSubscribed =
    Dec.field "indexSubscribed" decodeKey


decodeStates : Decoder States
decodeStates =
    Store.decode decodeUri decodeUri decodeState


decodeSubscriber : Decoder ( Key, State )
decodeSubscriber =
    Dec.map2 (,)
        (Dec.field "key" decodeKey)
        (Dec.field "state" decodeState)


decodeIndexSubscriber : Decoder Key
decodeIndexSubscriber =
    (Dec.field "index" decodeKey)


decodeKey : Decoder Key
decodeKey =
    Dec.map2 (,)
        (Dec.index 0 decodeUri)
        (Dec.index 1 decodeUri)


decodeLocalOperations : Decoder LocalOperations
decodeLocalOperations =
    Store.decode decodeUri decodeUri decodeCharOrRefOps


decodeCharOrRefOps : Decoder CharOrRefOps
decodeCharOrRefOps =
    Dec.oneOf
        [ Dec.map RefOps <| Dec.field "ref" <| Dec.list (decodeLocal decodeUri)
        , Dec.map CharOps <| Dec.field "literal" <| Dec.list (decodeLocal decodeChar)
        ]


decodeCharOrRefRemoteOps : Decoder ( CharOrRefRemoteOps, State )
decodeCharOrRefRemoteOps =
    Dec.map2 (,)
        (Dec.index 0
            (Dec.oneOf
                [ Dec.map RefRemoteOps <| Dec.field "ref" <| Dec.list (decodeRemote decodeUri)
                , Dec.map CharRemoteOps <| Dec.field "literal" <| Dec.list (decodeRemote decodeChar)
                ]
            )
        )
        (Dec.index 1 decodeState)


decodeLocal : Decoder a -> Decoder (Local a)
decodeLocal decoder =
    Dec.map2 (,)
        (Dec.index 0 <|
            Dec.map2 (,)
                (Dec.index 0 decodeUri)
                (Dec.index 1 Dec.int)
        )
        (Dec.index 1 (decodeOp decoder))


decodeRemoteOperations : Decoder RemoteOperations
decodeRemoteOperations =
    Store.decode decodeUri decodeUri decodeCharOrRefRemoteOps


decodeRemote : Decoder a -> Decoder (Remote a)
decodeRemote decoder =
    Dec.map3 (\a b c -> ( a, ( b, c ) ))
        (Dec.index 0 LSEQ.decodeID)
        (Dec.index 1 decodeUri)
        (Dec.index 2 (decodeOp decoder))


decodeState : Decoder State
decodeState =
    Dec.int


decodeOp : Decoder a -> Decoder (LSEQ.Op a)
decodeOp decoder =
    Dec.oneOf
        [ decodeInsert decoder
        , decodeRemove decoder
        ]


decodeInsert : Decoder a -> Decoder (LSEQ.Op a)
decodeInsert decoder =
    "i"
        := decoder
        |> Dec.map LSEQ.Insert


decodeRemove : Decoder a -> Decoder (LSEQ.Op a)
decodeRemove _ =
    Dec.string
        |> Dec.andThen
            (\s ->
                if s == "r" then
                    Dec.succeed LSEQ.Remove
                else
                    Dec.fail "unknown op"
            )



{-
   decodeChangedEntities : Decoder ChangedEntities
   decodeChangedEntities =
       Store.decode decodeUri decodeUri decodeChangedObject


   decodeChangedObject : Decoder ChangedObject
   decodeChangedObject =
       Dec.map2 (,)
           (Dec.index 0 <| LSEQ.decodeLayer Dec.string)
           (Dec.index 1 <| Dec.list decodeRemote)
-}


decodePredicate : Decoder ( Predicate, List String )
decodePredicate =
    Dec.map2 (,)
        (Dec.index 0 decodeUri)
        (Dec.index 1 <| Dec.list Dec.string)


decodeRequest : (Uri -> Key -> msg) -> (Uri -> Key -> msg) -> Decoder msg
decodeRequest requestMsg requestIndexMsg =
    Dec.index 0 decodeUri
        |> Dec.andThen
            (\uri ->
                Dec.index 1
                    (Dec.oneOf
                        [ Dec.field "r" decodeKey |> Dec.map (requestMsg uri)
                        , Dec.field "i" decodeKey |> Dec.map (requestIndexMsg uri)
                        ]
                    )
            )


decodeEntities : Decoder Entities
decodeEntities =
    Store.decode decodeUri decodeUri decodeObject


decodeObject : Decoder Object
decodeObject =
    Dec.oneOf
        [ Dec.map Ref <| Dec.field "ref" <| Dec.list <| LSEQ.decodeEntry decodeUri
        , Dec.map Literal <| Dec.field "literal" <| Dec.list <| LSEQ.decodeEntry decodeChar
        ]


decodeChar : Decoder Char
decodeChar =
    Dec.string
        |> Dec.andThen
            (\str ->
                case String.toList str |> List.head of
                    Nothing ->
                        Dec.fail "no char"

                    Just c ->
                        Dec.succeed c
            )


decodeRemoteOperationsAndState : Decoder ( RemoteOperations, State )
decodeRemoteOperationsAndState =
    Dec.map2 (,)
        (Dec.index 0 decodeRemoteOperations)
        (Dec.index 1 decodeState)
