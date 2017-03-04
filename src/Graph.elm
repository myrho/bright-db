module Graph exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)
import Tuple exposing (..)
import LSEQ exposing (LSEQ)
import LSEQ.Types as LSEQ


type alias State =
    Int


type alias Uri =
    String


type alias Subject =
    Uri


type alias Predicate =
    Uri


type alias Graph =
    Dict Subject Node


type alias Node =
    { id : Uri
    , incoming : Dict Predicate (Set Uri)
    , outgoing : Adjacency Uri
    , body : Adjacency Char
    }


type alias Adjacency a =
    Dict Predicate (LSEQ a)


type LocalsOrRemotes a
    = Locals (List (Local a))
    | Remotes (List (Remote a))


type alias Local a =
    ( ( Uri, Int ), LSEQ.Op a )


type alias Remote a =
    LSEQ.HistoryEntry a


empty : Graph
empty =
    Dict.empty


initNode : Subject -> Node
initNode s =
    Node s Dict.empty Dict.empty Dict.empty


get : Uri -> Graph -> Maybe Node
get uri graph =
    Dict.get uri graph


applyCharOps : Uri -> Subject -> Predicate -> LocalsOrRemotes Char -> Graph -> ( Graph, ( List (Remote Char), State ) )
applyCharOps origin s p ops graph =
    Dict.get s graph
        |> Maybe.withDefault (initNode s)
        |> (\node ->
                Dict.get p node.body
                    |> Maybe.withDefault LSEQ.empty
                    |> (case ops of
                            Locals ops ->
                                LSEQ.applyLocalOps origin ops

                            Remotes ops ->
                                LSEQ.applyRemoteOps ops
                       )
                    |> (\( lseq, ops ) ->
                            ( { node
                                | body =
                                    Dict.insert p lseq node.body
                              }
                            , ( ops, LSEQ.currentState lseq )
                            )
                       )
           )
        |> mapFirst (\node -> Dict.insert s node graph)


applyRefOps : Uri -> Subject -> Predicate -> LocalsOrRemotes Uri -> Graph -> ( Graph, ( List (Remote Uri), State ) )
applyRefOps origin s p ops graph =
    Dict.get s graph
        |> Maybe.withDefault (initNode s)
        |> (\node ->
                Dict.get p node.outgoing
                    |> Maybe.withDefault LSEQ.empty
                    |> (case ops of
                            Locals ops ->
                                LSEQ.applyLocalOps origin ops

                            Remotes ops ->
                                LSEQ.applyRemoteOps ops
                       )
                    |> (\( lseq, changes ) ->
                            ( { node
                                | outgoing =
                                    Dict.insert p lseq node.outgoing
                              }
                            , ( changes
                                    |> List.filterMap
                                        (\(( id, ( origin, op ) ) as change) ->
                                            case op of
                                                LSEQ.Remove ->
                                                    LSEQ.resurrect lseq id
                                                        |> Maybe.map ((,) change)

                                                LSEQ.Insert v ->
                                                    Just ( change, v )
                                        )
                              , LSEQ.currentState lseq
                              )
                            )
                       )
           )
        |> (\( node, ( changes, state ) ) ->
                ( Dict.insert s node graph
                    |> (\graph ->
                            List.foldl
                                (\( ( _, ( _, op ) ), ref ) graph ->
                                    Dict.get ref graph
                                        |> Maybe.withDefault (initNode ref)
                                        |> (\node ->
                                                Dict.get p node.incoming
                                                    |> Maybe.withDefault Set.empty
                                                    |> (case op of
                                                            LSEQ.Insert _ ->
                                                                Set.insert s

                                                            LSEQ.Remove ->
                                                                Set.remove s
                                                       )
                                                    |> (\set ->
                                                            { node
                                                                | incoming =
                                                                    Dict.insert p set node.incoming
                                                            }
                                                       )
                                                    |> (\node -> Dict.insert ref node graph)
                                           )
                                )
                                graph
                                changes
                       )
                , ( List.map first changes, state )
                )
           )
