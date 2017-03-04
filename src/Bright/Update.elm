module Bright.Update exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Task
import Tuple exposing (first, second, mapSecond)
import Array.Hamt as Array exposing (Array)
import List.Extra
import Bright.Model exposing (..)
import Bright.Uris as U
import Bright.Cmds
import Bright.Ports
import Bright.DB as DB exposing (Entities, Query, CharOrRefRemoteOps(..), CharOrRefOps(..), RemoteOperations, LocalOperations, wildcard, Meta, States)
import Bright.Encoder exposing (..)
import Tuple exposing (second, mapFirst)
import LSEQ.Types as LSEQ
import LSEQ
import Maybe.Extra
import Store
import Graph exposing (Uri, Subject, Predicate, Graph, State, Remote)


type Msg
    = Save App LocalOperations
    | Load Uri RemoteOperations
    | Error String
    | AddPeer Uri
    | ClosePeer Uri
    | LocalQuery Uri Query
    | RemoteQuery Peer Query
    | ReQuery Peer Query
    | Subscriber Peer States
    | ReceiveMeta Peer Meta
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "brightMsg" msg of
        NoOp ->
            ( model, Cmd.none )

        Save app localOps ->
            if Store.isEmpty localOps then
                ( model, Cmd.none )
            else
                let
                    ( db, remoteOps ) =
                        filterDisallowed app localOps
                            |> DB.mutate model.db app

                    model_ =
                        { model
                            | db = db
                        }
                in
                    ( model_
                    , notify model_ app remoteOps
                    )

        Error str ->
            ( model, Cmd.none )

        Load peer remoteOps ->
            if Store.isEmpty remoteOps then
                ( model, Cmd.none )
            else
                let
                    ( subscribed, complete, incomplete ) =
                        Store.foldl
                            (\s p ( ops, state ) ( subscribed, complete, incomplete ) ->
                                case Store.getObject s p subscribed of
                                    Just dict ->
                                        case Dict.get peer dict of
                                            Just oldState ->
                                                if oldState + (DB.opsLength ops) < state then
                                                    ( subscribed
                                                    , complete
                                                    , Store.insertObject s p oldState incomplete
                                                    )
                                                else
                                                    ( Store.insertObject s p (Dict.insert peer state dict) subscribed
                                                    , Store.insertObject s p ( ops, state ) complete
                                                    , incomplete
                                                    )

                                            Nothing ->
                                                ( Store.insertObject s p (Dict.singleton peer state) subscribed
                                                , Store.insertObject s p ( ops, state ) complete
                                                , incomplete
                                                )

                                    Nothing ->
                                        ( Store.insertObject s p (Dict.singleton peer state) subscribed
                                        , Store.insertObject s p ( ops, state ) complete
                                        , incomplete
                                        )
                            )
                            ( model.subscribed, Store.empty, Store.empty )
                            remoteOps

                    ( db, remoteOps_ ) =
                        DB.mutateRemote model.db complete

                    model_ =
                        { model
                            | subscribed = subscribed
                            , db = db
                        }
                in
                    ( model_
                    , Cmd.batch
                        [ notify model_ peer remoteOps_
                        , Bright.Cmds.subscribe peer incomplete
                        ]
                    )

        AddPeer peer ->
            let
                newModel =
                    { model
                        | peers = Set.insert peer model.peers
                    }
            in
                ( newModel
                , flattenQueries model.localQueries
                    |> Debug.log "flattenQueries"
                    |> List.map (Bright.Cmds.queryMeta (Set.toList newModel.peers))
                    |> Cmd.batch
                )

        ClosePeer peer ->
            ( { model
                | peers = Set.remove peer model.peers
              }
            , Cmd.none
            )

        LocalQuery app query ->
            let
                ( queries, unchanged ) =
                    storeQuery app query model.localQueries
                        |> Debug.log "localQueries"
            in
                if unchanged then
                    ( model, Cmd.none )
                else
                    ( { model
                        | localQueries = queries
                      }
                    , Cmd.batch
                        [ DB.query model.db query
                            |> Bright.Cmds.localResult app
                        , Bright.Cmds.queryMeta (Set.toList model.peers) query
                        ]
                    )

        RemoteQuery peer query ->
            Bright.Cmds.requeryMeta peer query
                |> remoteQuery model peer query

        ReQuery peer query ->
            remoteQuery model peer query Cmd.none

        ReceiveMeta peer meta ->
            ( model
            , Bright.Cmds.subscribeByMeta model peer meta
            )

        Subscriber peer states ->
            -- this action actually subscribes for updates AND triggers getPast
            -- could be separated!
            let
                result =
                    DB.getPast model.db states

                subscribers =
                    Store.foldl
                        (\s p _ subscribers ->
                            Dict.update peer
                                (Maybe.map (Store.insertObject s p ()))
                                subscribers
                        )
                        model.subscribers
                        states
            in
                ( { model
                    | subscribers = subscribers
                  }
                , if Store.isEmpty result then
                    Cmd.none
                  else
                    Bright.Cmds.remoteResult peer result
                )


remoteQuery : Model -> Peer -> Query -> Cmd Msg -> ( Model, Cmd Msg )
remoteQuery model peer query requery =
    let
        ( queries, unchanged ) =
            storeQuery peer query model.remoteQueries
                |> Debug.log "remoteQueries"
    in
        let
            current =
                getUrisForQuery query model.remoteQueries

            meta =
                DB.queryMeta model.db query

            sendMeta =
                if Dict.isEmpty meta then
                    Cmd.none
                else
                    Bright.Cmds.sendMeta peer meta
        in
            ( { model
                | remoteQueries = queries
              }
            , Cmd.batch
                [ sendMeta
                , requery
                ]
            )


notify : Model -> Uri -> RemoteOperations -> Cmd Msg
notify model origin remoteOps =
    let
        appsData =
            affectedQueries model.localQueries remoteOps
                |> Debug.log "affectedQueries"
                |> List.map
                    (\( query, apps ) ->
                        DB.query model.db query
                            |> (\entities ->
                                    Store.filter
                                        (\s _ ->
                                            Store.toDicts remoteOps
                                                |> Dict.member s
                                        )
                                        entities
                                        |> (,) (Set.remove origin apps)
                               )
                    )

        metaData =
            affectedQueries model.remoteQueries remoteOps
                |> Debug.log "affectedQueriesRemote"
                |> List.map
                    (\( query, peers ) ->
                        DB.queryMeta model.db query
                            |> (\meta ->
                                    Dict.filter
                                        (\s _ ->
                                            Store.toDicts remoteOps
                                                |> Dict.member s
                                        )
                                        (Debug.log "meta" meta)
                                        |> (,) (Set.remove origin peers)
                               )
                    )

        remoteResultCmds =
            model.subscribers
                |> Dict.map
                    (\peer subs ->
                        Store.map2
                            (\s p result () -> result)
                            remoteOps
                            subs
                            |> Bright.Cmds.remoteResult peer
                    )
                |> Dict.values
    in
        List.map notifyApps appsData
            ++ List.map notifyPeers metaData
            ++ remoteResultCmds
            |> Cmd.batch


notifyApps : ( Set App, Entities ) -> Cmd Msg
notifyApps ( apps, entities ) =
    Set.toList apps
        |> List.map (\app -> Bright.Cmds.localResult app entities)
        |> Cmd.batch


notifyPeers : ( Set Peer, Meta ) -> Cmd Msg
notifyPeers ( peers, meta ) =
    Set.toList peers
        |> List.map (\peer -> Bright.Cmds.sendMeta peer meta)
        |> Cmd.batch


affectedQueries : Queries -> RemoteOperations -> List ( Query, Set Uri )
affectedQueries queries remoteOps =
    let
        forQuery query =
            let
                set =
                    getUrisForQuery query queries
            in
                if Set.isEmpty set then
                    []
                else
                    [ ( query, set ) ]

        opsToQueries s p ops =
            case ops of
                CharRemoteOps _ ->
                    []

                RefRemoteOps ops ->
                    List.map
                        (\( _, ( _, op ) ) ->
                            case op of
                                LSEQ.Insert v ->
                                    (forQuery ( wildcard, p, v ))
                                        ++ (forQuery ( s, p, v ))

                                LSEQ.Remove ->
                                    []
                        )
                        ops
                        |> List.concat
    in
        remoteOps
            |> Store.toDicts
            |> Dict.foldl
                (\s entity result ->
                    Dict.foldl
                        (\p ( ops, _ ) result ->
                            (forQuery ( s, p, wildcard ))
                                ++ (opsToQueries s p ops)
                                ++ result
                        )
                        ((forQuery ( s, wildcard, wildcard ))
                            ++ (forQuery ( wildcard, wildcard, wildcard ))
                            ++ result
                        )
                        entity
                )
                []


filterDisallowed : Uri -> LocalOperations -> LocalOperations
filterDisallowed app =
    Store.filter (\s _ -> String.startsWith app s || String.startsWith userSpace s)
