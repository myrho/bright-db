module Bright.Model exposing (..)

import Dict exposing (Dict)
import Random exposing (Seed)
import Set exposing (Set)
import Maybe.Extra exposing (join)
import List.Extra
import Array.Hamt as Array exposing (Array)
import Bright.Uris exposing (..)
import LSEQ exposing (LSEQ)
import LSEQ.Types as LSEQ
import Store
import Bright.DB exposing (Entities, Query, wildcard)
import Graph exposing (Subject, Predicate, Graph, Uri, State, Remote, Local)
import Tuple exposing (mapFirst)


userSpace =
    "device"


{-|
Entities is derived from History
-}
type alias Model =
    { namespace : Uri
    , entities : Entities
    , db : Graph
    , peers : Set Uri
    , subscribers : Subscribers
    , subscribed : Subscribed
    , localQueries : Queries
    , remoteQueries : Queries
    , seed : Seed
    }


{-|
  Stores queries in a hierarchy of dicts: Subject > Predicate > (Object, Set App)
-}
type alias Queries =
    Store (Dict Uri (Set Uri))


type alias App =
    Uri


type alias Peer =
    Uri


type alias Store a =
    Store.Store Subject Predicate a


{-|
Subscriptions to other peers
key is the (subject, predicate) uri of the entity
-}
type alias Subscribed =
    Store (Dict Uri State)


{-|
-}
type alias Subscribers =
    Dict Peer (Store ())


type alias Subscription =
    ( Uri, Key, Int )


type alias Key =
    ( Subject, Predicate )


type Operation
    = Insert String
    | Remove


type alias Flags =
    { namespace : Uri
    , seed : Int
    }


init : Flags -> ( Model, Cmd msg )
init flags =
    ( initModel flags
    , Cmd.none
    )


initModel : Flags -> Model
initModel { namespace, seed } =
    Model namespace Store.empty Graph.empty Set.empty Dict.empty Store.empty Store.empty Store.empty <| Random.initialSeed seed


initEntities : Entities
initEntities =
    Store.empty


currentState : Model -> Uri -> Key -> State
currentState model peer ( s, p ) =
    Store.getObject s p model.subscribed
        |> Debug.log ("currentState subscribed " ++ s ++ ", " ++ p)
        |> Maybe.map (Dict.get peer)
        |> Maybe.Extra.join
        |> Maybe.withDefault -1


getSubscribers : Model -> Key -> ( Set Uri, Set Uri )
getSubscribers model ( subject, predicate ) =
    let
        ( allLocal, allRemote ) =
            ( Set.empty, Set.empty )

        ( specLocal, specRemote ) =
            ( Set.empty, Set.empty )
    in
        ( Set.union allLocal specLocal
        , Set.union allRemote specRemote
        )


getSubscriptions : Model -> Key -> Dict Uri State
getSubscriptions model ( subject, predicate ) =
    let
        all =
            Store.getObject subject wildcard model.subscribed
                |> Maybe.withDefault Dict.empty

        spec =
            Store.getObject subject predicate model.subscribed
                |> Maybe.withDefault Dict.empty
    in
        Dict.union all spec


mapOp : LSEQ.Op String -> Operation
mapOp op =
    case op of
        LSEQ.Insert str ->
            Insert str

        LSEQ.Remove ->
            Remove


getUrisForQuery : Query -> Queries -> Set Uri
getUrisForQuery ( s, p, o ) queries =
    Store.getObject s p queries
        |> Maybe.withDefault Dict.empty
        |> Dict.get o
        |> Maybe.withDefault Set.empty


{-|
  Stores a query and associates it with an app. Returns the updated queries and a flag whether there where NO changes.
-}
storeQuery : App -> Query -> Queries -> ( Queries, Bool )
storeQuery app ( s, p, o ) queries =
    Store.getObject s p queries
        |> Maybe.withDefault Dict.empty
        |> (\dict ->
                Dict.get o dict
                    |> Maybe.withDefault Set.empty
                    |> (\set ->
                            ( Dict.insert o (Set.insert app set) dict
                            , Set.member app set
                            )
                       )
           )
        |> mapFirst (\dict -> Store.insertObject s p dict queries)


flattenQueries : Queries -> List Query
flattenQueries =
    Store.foldl
        (\s p dict flat ->
            Dict.foldl
                (\o _ flat ->
                    ( s, p, o ) :: flat
                )
                flat
                dict
        )
        []
