module Bright.DB exposing (..)

{-|
@docs CharOrRefOps,CharOrRefRemoteOps,Entities,Entity,LocalOperations,Meta,Object,Query,RemoteOperations,States,Store,getPast,mutate,mutateRemote,noRemotes,opsLength,query,queryMeta,wildcard
-}

import Graph exposing (Graph, Subject, Predicate, Uri, State)
import Set exposing (Set)
import Dict exposing (Dict)
import Store
import LSEQ exposing (LSEQ)
import LSEQ.Types as LSEQ
import Task exposing (Task)
import Tuple exposing (..)
import Array.Hamt as Array


{-|
-}
type alias States =
    Store State


{-|
-}
type alias RemoteOperations =
    Store ( CharOrRefRemoteOps, Graph.State )


{-|
-}
type alias LocalOperations =
    Store CharOrRefOps


{-|
-}
type CharOrRefOps
    = CharOps (List (Graph.Local Char))
    | RefOps (List (Graph.Local Uri))


{-|
-}
type CharOrRefRemoteOps
    = CharRemoteOps (List (Graph.Remote Char))
    | RefRemoteOps (List (Graph.Remote Uri))


{-|
-}
type Object
    = Ref (List (LSEQ.Entry Uri))
    | Literal (List (LSEQ.Entry Char))


{-|
-}
wildcard : String
wildcard =
    "*"


{-|
-}
type alias Store a =
    Store.Store Subject Predicate a


{-|
  A list of objects because a predicate could be used more than once
-}
type alias Entities =
    Store Object


{-|
-}
type alias Meta =
    Dict Subject (Set Predicate)


{-|
-}
type alias Entity =
    Dict Predicate Object


{-|
-}
type alias Query =
    ( Uri, Uri, Uri )


{-|
-}
opsLength : CharOrRefRemoteOps -> Int
opsLength ops =
    case ops of
        CharRemoteOps ops ->
            List.length ops

        RefRemoteOps ops ->
            List.length ops


{-|
-}
queryMeta : Graph -> Query -> Meta
queryMeta graph (( s, p, o ) as q) =
    if s == wildcard && p == wildcard && o == wildcard then
        Dict.map
            (\s node ->
                Dict.keys node.outgoing
                    ++ Dict.keys node.body
                    |> Set.fromList
            )
            graph
    else if s == wildcard && p /= wildcard && o /= wildcard then
        case Graph.get o graph of
            Nothing ->
                Dict.empty

            Just node ->
                node.incoming
                    |> Dict.foldl
                        (\p_ subjects result ->
                            if p /= p_ then
                                result
                            else
                                subjects
                                    |> Set.foldl
                                        (\s result ->
                                            case Graph.get s graph of
                                                Nothing ->
                                                    result

                                                Just node ->
                                                    if Dict.member p node.body || Dict.member p node.outgoing then
                                                        Dict.update s
                                                            (Maybe.withDefault Set.empty
                                                                >> Set.insert p
                                                                >> Just
                                                            )
                                                            result
                                                    else
                                                        result
                                        )
                                        result
                        )
                        Dict.empty
    else if s /= wildcard && o == wildcard then
        case Graph.get s graph of
            Nothing ->
                Dict.empty

            Just node ->
                (Dict.keys node.body ++ Dict.keys node.outgoing)
                    |> Set.fromList
                    |> Dict.singleton s
    else
        Dict.empty


{-|
-}
query : Graph -> Query -> Entities
query graph (( s, p, o ) as q) =
    let
        foldOutgoing ( s, p, o ) outgoing result =
            Dict.foldl
                (\p_ lseqUri result ->
                    if p /= wildcard && p_ /= p then
                        result
                    else
                        Store.insertObject s p_ (Ref <| LSEQ.toList lseqUri) result
                )
                result
                outgoing

        foldBody ( s, p, o ) body result =
            Dict.foldl
                (\p_ lseqChar result ->
                    if p /= wildcard && p_ /= p then
                        result
                    else
                        Store.insertObject s p_ (Literal <| LSEQ.toList lseqChar) result
                )
                result
                body
    in
        if s == wildcard && p /= wildcard && o /= wildcard then
            case Graph.get o graph of
                Nothing ->
                    Store.empty

                Just node ->
                    node.incoming
                        |> Dict.foldl
                            (\p_ subjects result ->
                                if p /= p_ then
                                    result
                                else
                                    subjects
                                        |> Set.foldl
                                            (\s result ->
                                                case Graph.get s graph of
                                                    Nothing ->
                                                        result

                                                    Just node ->
                                                        case Dict.get p node.body of
                                                            Nothing ->
                                                                case Dict.get p node.outgoing of
                                                                    Nothing ->
                                                                        result

                                                                    Just lseqUri ->
                                                                        Store.insertObject s p (Ref <| LSEQ.toList lseqUri) result

                                                            Just lseqChar ->
                                                                Store.insertObject s p (Literal <| LSEQ.toList lseqChar) result
                                            )
                                            result
                            )
                            Store.empty
        else if s /= wildcard && o == wildcard then
            case Graph.get s graph of
                Nothing ->
                    Store.empty

                Just node ->
                    foldOutgoing q node.outgoing Store.empty
                        |> foldBody q node.body
        else
            Store.empty


{-|
-}
mutate : Graph -> Uri -> LocalOperations -> ( Graph, RemoteOperations )
mutate graph origin localOps =
    localOps
        |> Store.foldl
            (\s p locals ( graph, remoteOps ) ->
                case locals of
                    CharOps locals ->
                        Graph.applyCharOps origin s p (Graph.Locals locals) graph
                            |> mapSecond
                                (\( remotes, state ) ->
                                    if List.isEmpty remotes then
                                        remoteOps
                                    else
                                        Store.insertObject s p ( (CharRemoteOps remotes), state ) remoteOps
                                )

                    RefOps locals ->
                        Graph.applyRefOps origin s p (Graph.Locals locals) graph
                            |> mapSecond
                                (\( remotes, state ) ->
                                    if List.isEmpty remotes then
                                        remoteOps
                                    else
                                        Store.insertObject s p ( (RefRemoteOps remotes), state ) remoteOps
                                )
            )
            ( graph, Store.empty )


{-|
-}
mutateRemote : Graph -> RemoteOperations -> ( Graph, RemoteOperations )
mutateRemote graph remoteOps =
    remoteOps
        |> Store.foldl
            (\s p ( remotes, _ ) ( graph, remoteOps ) ->
                case remotes of
                    CharRemoteOps remotes ->
                        Graph.applyCharOps "" s p (Graph.Remotes remotes) graph
                            |> mapSecond
                                (\( remotes, state ) ->
                                    if List.isEmpty remotes then
                                        remoteOps
                                    else
                                        Store.insertObject s p ( (CharRemoteOps remotes), state ) remoteOps
                                )

                    RefRemoteOps remotes ->
                        Graph.applyRefOps "" s p (Graph.Remotes remotes) graph
                            |> mapSecond
                                (\( remotes, state ) ->
                                    if List.isEmpty remotes then
                                        remoteOps
                                    else
                                        Store.insertObject s p ( (RefRemoteOps remotes), state ) remoteOps
                                )
            )
            ( graph, Store.empty )


{-|
-}
noRemotes : ( CharOrRefRemoteOps, State )
noRemotes =
    ( CharRemoteOps [], -1 )


{-|
-}
getPast : Graph -> States -> RemoteOperations
getPast graph states =
    states
        |> Store.map
            (\s p state ->
                case Graph.get s graph of
                    Nothing ->
                        noRemotes

                    Just node ->
                        case Dict.get p node.body of
                            Nothing ->
                                case Dict.get p node.outgoing of
                                    Nothing ->
                                        noRemotes

                                    Just lseqUri ->
                                        ( LSEQ.Offset state
                                            |> LSEQ.getPast lseqUri
                                            |> Array.toList
                                            |> RefRemoteOps
                                        , LSEQ.currentState lseqUri
                                        )

                            Just lseqChar ->
                                ( LSEQ.Offset state
                                    |> LSEQ.getPast lseqChar
                                    |> Array.toList
                                    |> CharRemoteOps
                                , LSEQ.currentState lseqChar
                                )
            )
