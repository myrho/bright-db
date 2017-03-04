module Bright.Encoder exposing (..)

import Set exposing (Set)
import Dict
import Bright.Model exposing (..)
import Bright.DB exposing (Entities, Object(..), Query, LocalOperations, RemoteOperations, CharOrRefRemoteOps(..), CharOrRefOps(..), Meta, States)
import Graph exposing (Subject, Predicate, Uri, Local, Remote, State)
import Json.Encode as Enc
import LSEQ.Types as LSEQ
import LSEQ as LSEQ
import Store


encodeStates : States -> Enc.Value
encodeStates =
    Store.encode encodeUri encodeUri encodeState


encodeMeta : Meta -> Enc.Value
encodeMeta meta =
    Dict.toList meta
        |> List.map
            (\( s, ps ) ->
                Enc.list
                    [ encodeUri s
                    , encodePredicateSet ps
                    ]
            )
        |> Enc.list


encodePredicateSet : Set Predicate -> Enc.Value
encodePredicateSet =
    Set.toList >> List.map encodeUri >> Enc.list


encodeQuery : Query -> Enc.Value
encodeQuery query =
    Enc.object
        [ ( "q", encodeQuery_ query )
        ]


encodeQuery_ : Query -> Enc.Value
encodeQuery_ ( s, p, o ) =
    Enc.list [ Enc.string s, Enc.string p, Enc.string o ]


encodeRequery : Query -> Enc.Value
encodeRequery query =
    Enc.object
        [ ( "r", encodeQuery_ query )
        ]


encodeEntities : Entities -> Enc.Value
encodeEntities =
    Store.encode encodeUri encodeUri encodeObject


encodeObject : Object -> Enc.Value
encodeObject object =
    case object of
        Ref list ->
            Enc.object
                [ ( "ref", Enc.list <| List.map (LSEQ.encodeEntry Enc.string) list )
                ]

        Literal list ->
            Enc.object
                [ ( "literal", Enc.list <| List.map (LSEQ.encodeEntry encodeChar) list )
                ]


encodeChar : Char -> Enc.Value
encodeChar =
    String.fromChar >> Enc.string


encodeChangedEntities : ChangedEntities -> Enc.Value
encodeChangedEntities =
    Store.encode encodeUri encodeUri encodeChangedObject


encodeChangedObject : ChangedObject -> Enc.Value
encodeChangedObject ( current, remotes ) =
    Enc.list
        [ LSEQ.encodeLayer Enc.string current
          --, Enc.list <| List.map encodeRemote remotes
        , Enc.list []
        ]


encodeLocalOperations : LocalOperations -> Enc.Value
encodeLocalOperations =
    Store.encode encodeUri encodeUri encodeCharOrRefOps


encodeIndexOperations : IndexOperations -> Enc.Value
encodeIndexOperations =
    Store.encode encodeUri encodeUri (Enc.list << List.map encodeUri)


encodeRemoteOperations : RemoteOperations -> Enc.Value
encodeRemoteOperations =
    Store.encode encodeUri encodeUri encodeCharOrRefRemoteOps


encodeCharOrRefOps : CharOrRefOps -> Enc.Value
encodeCharOrRefOps ops =
    case ops of
        CharOps list ->
            Enc.object
                [ ( "literal", (Enc.list << List.map (encodeLocal encodeChar)) list )
                ]

        RefOps list ->
            Enc.object
                [ ( "ref", (Enc.list << List.map (encodeLocal encodeUri)) list )
                ]


encodeCharOrRefRemoteOps : ( CharOrRefRemoteOps, State ) -> Enc.Value
encodeCharOrRefRemoteOps ( ops, state ) =
    Enc.list
        [ case ops of
            CharRemoteOps list ->
                Enc.object
                    [ ( "literal"
                      , Enc.list <| List.map (encodeRemote encodeChar) list
                      )
                    ]

            RefRemoteOps list ->
                Enc.object
                    [ ( "ref"
                      , Enc.list <| List.map (encodeRemote encodeUri) list
                      )
                    ]
        , encodeState state
        ]


encodeLocal : (a -> Enc.Value) -> Local a -> Enc.Value
encodeLocal encoder ( ( target, pos ), op ) =
    Enc.list
        [ Enc.list [ encodeUri target, Enc.int pos ]
        , encodeOp op encoder
        ]



{-
   encodeLocal : Local -> Enc.Value
   encodeLocal ( pos, concurrents ) =
       Enc.list
           [ Enc.int pos
           , concurrents
               |> List.map
                   (\( target, op ) ->
                       Enc.list
                           [ encodeUri target
                           , encodeOp op
                           ]
                   )
               |> Enc.list
           ]
-}


encodeUri : Uri -> Enc.Value
encodeUri =
    Enc.string


encodeSubscribed : ( Subject, Predicate ) -> Enc.Value
encodeSubscribed key =
    Enc.object
        [ ( "subscribed", encodeKey key )
        ]


encodeIndexSubscribed : ( Subject, Predicate ) -> Enc.Value
encodeIndexSubscribed key =
    Enc.object
        [ ( "indexSubscribed", encodeKey key )
        ]


encodeKey : ( Subject, Predicate ) -> Enc.Value
encodeKey ( s, p ) =
    Enc.list
        [ encodeUri s
        , encodeUri p
        ]


encodePeer : Uri -> Enc.Value -> Enc.Value
encodePeer peer body =
    Enc.list
        [ encodeUri peer
        , body
        ]


encodeApp =
    encodePeer


encodeSubscription : Key -> Int -> Enc.Value
encodeSubscription key state =
    Enc.object
        [ ( "key", encodeKey key )
        , ( "state", Enc.int state )
        ]


encodeIndexSubscription : Key -> Enc.Value
encodeIndexSubscription key =
    Enc.object
        [ ( "index", encodeKey key )
        ]


encodeState : State -> Enc.Value
encodeState =
    Enc.int


encodeOp : LSEQ.Op a -> (a -> Enc.Value) -> Enc.Value
encodeOp op encoder =
    case op of
        LSEQ.Insert s ->
            encoder s
                |> (,) "i"
                |> (\x -> [ x ])
                |> Enc.object

        LSEQ.Remove ->
            Enc.string "r"


encodeRemote : (a -> Enc.Value) -> Remote a -> Enc.Value
encodeRemote encoder ( id, ( origin, op ) ) =
    Enc.list
        [ LSEQ.encodeID id
        , encodeUri origin
        , encodeOp op encoder
        ]
