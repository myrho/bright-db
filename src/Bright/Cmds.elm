module Bright.Cmds exposing (..)

import Dict
import Set
import Array.Hamt as Array
import Json.Encode as Enc
import Tuple exposing (first, second)
import List.Extra
import Bright.Encoder exposing (..)
import Bright.Model exposing (..)
import Bright.Util exposing (..)
import Bright.Ports
import Bright.Uris as U
import Graph exposing (Subject, Predicate, Uri, State)
import Bright.DB exposing (Entities, Query, RemoteOperations, Meta, States)
import LSEQ
import Store


subscribeByMeta : Model -> Uri -> Meta -> Cmd msg
subscribeByMeta model peer meta =
    Dict.foldl
        (\s ps states ->
            Set.toList ps
                |> List.foldl
                    (\p ->
                        Store.insertObject s p (currentState model peer ( s, p ))
                    )
                    states
        )
        Store.empty
        meta
        |> subscribe peer


subscribe : Uri -> States -> Cmd msg
subscribe peer =
    encodeStates
        >> encodePeer peer
        >> Bright.Ports.send


localResult : App -> Entities -> Cmd msg
localResult app entities =
    encodeEntities entities
        |> encodeApp app
        |> Bright.Ports.appLoad


remoteResult : Peer -> RemoteOperations -> Cmd msg
remoteResult peer result =
    encodeRemoteOperations result
        |> encodePeer peer
        |> Bright.Ports.send


queryMeta : List Peer -> Query -> Cmd msg
queryMeta peers query =
    let
        encoded =
            encodeQuery query
    in
        peers
            |> List.map
                (\peer ->
                    encodePeer peer encoded
                        |> Bright.Ports.send
                )
            |> Cmd.batch


requeryMeta : Peer -> Query -> Cmd msg
requeryMeta peer query =
    encodeRequery query
        |> encodePeer peer
        |> Bright.Ports.send


sendMeta : Peer -> Meta -> Cmd msg
sendMeta peer meta =
    encodeMeta meta
        |> encodePeer peer
        |> Bright.Ports.send
