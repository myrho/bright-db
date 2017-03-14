module Bright exposing (..)

import Html
import Task exposing (Task)
import Bright.Model exposing (..)
import Bright.Encoder exposing (encodeLocalOperations)
import Bright.Decoder exposing (decodeLocalOperations)
import Bright.Uris as U
import Bright.View
import Bright.IO
import Bright.Model
import Bright.Update
import Bright.Sub
import Bright.DB exposing (Object(..))
import Graph exposing (Predicate, Subject, Local)
import Array.Hamt as Array exposing (Array)
import Maybe.Extra
import List.Extra
import String.Extra
import Dict exposing (Dict)
import LSEQ
import LSEQ.Types as LSEQ
import Tuple exposing (..)
import String
import Store
import Diff


type alias Uri =
    String


type alias BBool =
    LSEQ.Entry Bool


type alias BFloat =
    List (LSEQ.Entry Char)


type alias BRef =
    LSEQ.Entry Uri


type alias BString =
    List (LSEQ.Entry Char)


type alias Object =
    Bright.DB.Object


type alias Entity =
    Bright.DB.Entity


type alias Entities =
    Bright.DB.Entities


class : Uri -> ( Predicate, List (Local Uri) )
class uri =
    ( U.isA
    , [ ( ( "", 0 ), LSEQ.Insert uri )
      ]
    )


insert : Int -> a -> List (Local a)
insert pos a =
    [ ( ( "", pos ), LSEQ.Insert a )
    ]


string : String -> List (Local Char)
string string =
    String.toList string
        |> List.indexedMap
            (\i -> (,) ( "", i ) >> (mapSecond LSEQ.Insert))


bool : Bool -> List (Local Char)
bool new =
    [ ( ( "", 0 )
      , LSEQ.Insert <|
            if new then
                '1'
            else
                '0'
      )
    ]


float : Float -> List (Local Char)
float new =
    toString new
        |> string


ref : Uri -> List (Local Uri)
ref uri =
    [ ( ( "", 0 )
      , LSEQ.Insert uri
      )
    ]


deleteRef : Uri -> List (Local Uri)
deleteRef target =
    [ ( ( target, 0 )
      , LSEQ.Remove
      )
    ]


{-| Compares input string and current string and creates an operation given the first char of the two string that do not match. If the input string is shorter than the current string, it's a remove operation, if longer, it's an insert operation, if equal it's a noop.
char : String -> String -> List Local
char current string =
    diffOperation String.fromChar (String.toList current) <|
        String.toList string
uri : List Uri -> List Uri -> List Local
uri current list =
    diffOperation identity current list
-}
remove : ( Uri, Int ) -> List (Local Uri)
remove id =
    [ ( id, LSEQ.Remove )
    ]


removeAll : List (LSEQ.Entry Char) -> List (Local Char)
removeAll chars =
    List.foldl
        (\entry ( locals, i ) ->
            case entry of
                LSEQ.Single o (LSEQ.Value v) ->
                    ( ( ( o, i ), LSEQ.Remove ) :: locals
                    , i + 1
                    )

                LSEQ.MVR dict ->
                    Dict.foldl
                        (\t v locals ->
                            case v of
                                LSEQ.Value v ->
                                    ( ( t, i ), LSEQ.Remove ) :: locals

                                _ ->
                                    locals
                        )
                        locals
                        dict
                        |> (\locals -> ( locals, i + 1 ))

                _ ->
                    ( locals, i )
        )
        ( [], 0 )
        chars
        |> first



{-
   diffOperation : (a -> String) -> List a -> List a -> List Local
   diffOperation toString chars1 chars2 =
       let
           len1 =
               List.length chars1

           len2 =
               List.length chars2

           pos =
               List.map2 (,) chars1 chars2
                   |> List.Extra.findIndex
                       (\( a, b ) -> a /= b)
                   |> Maybe.withDefault (min len1 len2)
       in
           if len1 > len2 then
               [ ( pos, LSEQ.Remove )
               ]
           else if len1 < len2 then
               case List.Extra.getAt pos chars2 of
                   Nothing ->
                       []

                   Just char ->
                       [ ( pos, LSEQ.Insert (toString char) )
                       ]
           else
               []
-}


isOf : String -> Entity -> Bool
isOf uri entity =
    let
        byValue uri item =
            case item of
                LSEQ.Single _ (LSEQ.Value u) ->
                    u == uri

                LSEQ.MVR mvr ->
                    Dict.values mvr |> List.member (LSEQ.Value uri)

                _ ->
                    False
    in
        case Dict.get U.isA entity of
            Just (Ref values) ->
                List.Extra.find (byValue uri) values |> (/=) Nothing

            _ ->
                False



{-
      applyLocalStringOps : List Local -> String -> String
      applyLocalStringOps locals string =
          List.foldl
              (\( pos, op ) string ->
                  case op of
                      LSEQ.Insert a ->
                          String.Extra.insertAt a pos string

                      LSEQ.Remove ->
                          String.Extra.replaceSlice "" pos (pos + 1) string
              )
              string
              locals

   applyLocalBoolOps : List Local -> Bool -> Bool
   applyLocalBoolOps locals bool =
       applyLocalStringOps locals
           (if bool then
               "1"
            else
               "0"
           )
           |> (==) "1"


   applyLocalUriOps : List Local -> (Uri -> a) -> List a -> List a
   applyLocalUriOps locals init list =
       List.foldl
           (\( pos, op ) list ->
               case op of
                   LSEQ.Insert uri ->
                       List.drop pos list
                           |> (::) (init uri)
                           |> (++) (List.take pos list)

                   LSEQ.Remove ->
                       List.Extra.removeAt pos list
           )
           list
           locals

-}


valueOnly2String : List (LSEQ.ValueOnly String) -> String
valueOnly2String =
    List.foldl
        (\value string ->
            case value of
                LSEQ.SingleValue _ v ->
                    string ++ v

                LSEQ.Concurrent mvr ->
                    Dict.toList mvr
                        |> List.sortBy first
                        |> List.head
                        |> Maybe.map second
                        |> Maybe.withDefault ""
                        |> (++) string
        )
        ""


initBBool : Uri -> Bool -> BBool
initBBool origin bool =
    LSEQ.Single origin (LSEQ.Value bool)


initBFloat : Uri -> Float -> BFloat
initBFloat origin fl =
    toString fl
        |> String.toList
        |> List.map (LSEQ.Value >> LSEQ.Single origin)


initBString : Uri -> String -> BString
initBString =
    string2ValueList


toBBool : Object -> Maybe BBool
toBBool object =
    case object of
        Ref _ ->
            Nothing

        Literal list ->
            List.head list
                |> Maybe.map
                    (LSEQ.mapEntry
                        (\v ->
                            if v == '1' then
                                True
                            else
                                False
                        )
                    )


toBRef : Object -> Maybe BRef
toBRef object =
    case object of
        Ref list ->
            List.head list

        Literal list ->
            Nothing


toBString : Object -> BString
toBString object =
    case object of
        Ref list ->
            []

        Literal list ->
            list


toBFloat : Object -> BFloat
toBFloat =
    toBString


bRefToUri : Uri -> BRef -> Maybe Uri
bRefToUri origin ref =
    case ref of
        LSEQ.Single _ (LSEQ.Value uri) ->
            Just uri

        LSEQ.MVR mvr ->
            case Dict.get origin mvr of
                Just (LSEQ.Value uri) ->
                    Just uri

                _ ->
                    (Dict.toList mvr
                        |> List.sortBy first
                        |> List.head
                        |> Maybe.map second
                        |> Maybe.map
                            (\v ->
                                case v of
                                    LSEQ.Value uri ->
                                        Just uri

                                    _ ->
                                        Nothing
                            )
                        |> Maybe.Extra.join
                    )

        _ ->
            Nothing


bFloatToFloat : Uri -> BFloat -> Result String Float
bFloatToFloat origin float =
    purge float
        |> List.map
            (\c ->
                case c of
                    LSEQ.SingleValue _ b ->
                        String.fromChar b

                    LSEQ.Concurrent mvr ->
                        case Dict.get origin mvr of
                            Just b ->
                                String.fromChar b

                            Nothing ->
                                (Dict.toList mvr
                                    |> List.sortBy first
                                    |> List.head
                                    |> Maybe.map second
                                    |> Maybe.map String.fromChar
                                    |> Maybe.withDefault ""
                                )
            )
        |> String.concat
        |> String.toFloat


bBoolToBool : Uri -> BBool -> Bool
bBoolToBool origin bool =
    case bool of
        LSEQ.Single _ (LSEQ.Value b) ->
            b

        LSEQ.MVR mvr ->
            case Dict.get origin mvr of
                Just (LSEQ.Value b) ->
                    b

                _ ->
                    (Dict.toList mvr
                        |> List.sortBy first
                        |> List.head
                        |> Maybe.map second
                        |> Maybe.map
                            (\v ->
                                case v of
                                    LSEQ.Value b ->
                                        b

                                    _ ->
                                        False
                            )
                        |> Maybe.withDefault False
                    )

        _ ->
            False


bStringToString : Uri -> BString -> String
bStringToString origin string =
    let
        value2String v =
            case v of
                LSEQ.Tomb _ ->
                    Nothing

                LSEQ.Value v ->
                    Just <| String.fromChar v
    in
        string
            |> List.foldl
                (\value str ->
                    case value of
                        LSEQ.Single _ v ->
                            str ++ (value2String v |> Maybe.withDefault "")

                        LSEQ.MVR mvr ->
                            Dict.get origin mvr
                                |> Maybe.map value2String
                                |> Maybe.Extra.join
                                |> Maybe.withDefault
                                    (Dict.toList mvr
                                        |> List.sortBy first
                                        |> List.foldl
                                            (\( origin, value ) result ->
                                                case result of
                                                    Nothing ->
                                                        value2String value

                                                    Just _ ->
                                                        result
                                            )
                                            Nothing
                                        |> Maybe.withDefault ""
                                    )
                                |> (++) str
                )
                ""


valueList2LocalOps : List (LSEQ.Entry a) -> List (Local a)
valueList2LocalOps list =
    list
        |> List.indexedMap value2LocalOp
        |> List.filterMap identity


value2LocalOp : Int -> LSEQ.Entry a -> Maybe (Local a)
value2LocalOp pos value =
    case value of
        LSEQ.Single target (LSEQ.Value v) ->
            Just ( ( target, pos ), LSEQ.Insert v )

        _ ->
            Nothing


purge : List (LSEQ.Entry a) -> List (LSEQ.ValueOnly a)
purge =
    List.filterMap
        (\v ->
            case v of
                LSEQ.Single target (LSEQ.Value a) ->
                    Just <| LSEQ.SingleValue target a

                LSEQ.Single target (LSEQ.Tomb a) ->
                    Nothing

                LSEQ.MVR mvr ->
                    Dict.toList mvr
                        |> List.filterMap
                            (\( k, v ) ->
                                case v of
                                    LSEQ.Value a ->
                                        Just ( k, a )

                                    _ ->
                                        Nothing
                            )
                        |> (\list ->
                                if List.isEmpty list then
                                    Nothing
                                else
                                    Dict.fromList list
                                        |> LSEQ.Concurrent
                                        |> Just
                           )
        )


toList : List (LSEQ.Entry Uri) -> List Uri
toList entryList =
    entryList
        |> List.filterMap
            (\entry ->
                case entry of
                    LSEQ.Single _ (LSEQ.Value uri) ->
                        Just [ uri ]

                    LSEQ.MVR dict ->
                        Dict.toList dict
                            |> List.filterMap
                                (\( k, v ) ->
                                    case v of
                                        LSEQ.Value uri ->
                                            Just uri

                                        _ ->
                                            Nothing
                                )
                            |> Just

                    _ ->
                        Nothing
            )
        |> List.concat


loadByType : Entities -> model -> List ( Uri, model -> Subject -> Entity -> ( model, Cmd msg ) ) -> ( model, Cmd msg )
loadByType entities model map =
    Store.toDicts entities
        |> Dict.foldl
            (\s entity ( model, cmds ) ->
                List.foldl
                    (\( typeUri, load ) ( model, cmds ) ->
                        (if isOf typeUri entity then
                            load model s entity
                         else
                            ( model, Cmd.none )
                        )
                            |> mapSecond (\cmd -> cmd :: cmds)
                    )
                    ( model, cmds )
                    map
            )
            ( model, [] )
        |> mapSecond Cmd.batch


load : model -> List ( Uri, model -> Object -> ( model, Cmd msg ) ) -> Entity -> ( model, Cmd msg )
load model map =
    Dict.foldl
        (\p current ( model, cmds ) ->
            List.foldl
                (\( predicate, load ) ( model, cmds ) ->
                    (if p == predicate then
                        load model current
                     else
                        ( model, Cmd.none )
                    )
                        |> mapSecond (\cmd -> cmd :: cmds)
                )
                ( model, cmds )
                map
        )
        ( model, [] )
        >> mapSecond Cmd.batch


find : Uri -> List { a | uri : Subject } -> Maybe { a | uri : Subject }
find s =
    List.Extra.find (predicate s)


replace : Uri -> { a | uri : Subject } -> List { a | uri : Subject } -> List { a | uri : Subject }
replace s =
    List.Extra.replaceIf (predicate s)


removeByUri : Uri -> List { a | uri : Subject } -> List { a | uri : Subject }
removeByUri uri =
    List.filter (.uri >> (/=) uri)


predicate : Uri -> { a | uri : Subject } -> Bool
predicate uri =
    (.uri >> (==) uri)


emptyBString : BString
emptyBString =
    []


loadUnorderedListItem :
    (Subject -> { list | uri : Subject })
    -> (Subject -> { item | uri : Subject })
    -> ({ list | uri : Subject } -> List { item | uri : Subject })
    -> ({ list | uri : Subject } -> List { item | uri : Subject } -> { list | uri : Subject })
    -> (Uri -> Object -> { item | uri : Subject } -> { item | uri : Subject })
    -> Uri
    -> List { list | uri : Subject }
    -> Subject
    -> Entity
    -> ( List { list | uri : Subject }, Cmd msg )
loadUnorderedListItem initList initItem getSublist updateSublist foldItem parentListUri listOfLists s entity =
    let
        ( oldLists, item ) =
            listOfLists
                |> List.foldl
                    (\list ( found, item ) ->
                        case find s <| getSublist list of
                            Just f ->
                                ( list :: found, f )

                            Nothing ->
                                ( found, item )
                    )
                    ( [], initItem s )

        ( newLists, cmds ) =
            Dict.get parentListUri entity
                |> Maybe.map toBRef
                |> Maybe.Extra.join
                |> toList
                |> List.unzip

        toList object =
            case object of
                Just (LSEQ.Single _ (LSEQ.Value listUri)) ->
                    [ List.Extra.find (predicate listUri) listOfLists
                        |> Maybe.map (\list -> ( ( list, False ), Cmd.none ))
                        |> Maybe.withDefault
                            ( ( initList listUri, True )
                            , Cmd.batch
                                [ Bright.IO.query ( listUri, "*", "*" )
                                ]
                            )
                    ]

                Just (LSEQ.MVR mvr) ->
                    Dict.values mvr
                        |> List.filterMap
                            (\listUri ->
                                case listUri of
                                    LSEQ.Tomb _ ->
                                        Nothing

                                    LSEQ.Value listUri ->
                                        find listUri listOfLists
                                            |> Maybe.map (\list -> ( ( list, False ), Cmd.none ))
                                            |> Maybe.withDefault
                                                ( ( initList listUri, True )
                                                , Cmd.batch
                                                    [ Bright.IO.query ( listUri, "*", "*" )
                                                    ]
                                                )
                                            |> Just
                            )

                _ ->
                    []

        findInSublist s list =
            getSublist list
                |> find s
                |> (/=) Nothing

        newItem =
            Dict.foldl foldItem item entity
                |> Debug.log "newItem"

        ( oldLists_, newLists_, unchangedLists ) =
            ( List.filter (\list -> List.Extra.find (first >> predicate list.uri) newLists |> (==) Nothing) oldLists
            , List.filter (\( list, _ ) -> List.Extra.find (predicate list.uri) oldLists |> (==) Nothing) newLists
            , List.filter (\( list, _ ) -> List.Extra.find (predicate list.uri) oldLists |> (/=) Nothing) newLists
            )

        oldLists__ =
            List.map
                (\list ->
                    getSublist list
                        |> List.filter (.uri >> (/=) s)
                        |> updateSublist list
                )
                oldLists_
                |> Debug.log "oldTodolists"

        newLists__ =
            List.map
                (mapFirst
                    (\list ->
                        getSublist list
                            |> (::) newItem
                            |> updateSublist list
                    )
                )
                newLists_
                |> Debug.log "newTodolists"

        unchangedLists_ =
            List.map
                (mapFirst
                    (\list ->
                        getSublist list
                            |> replace s newItem
                            |> updateSublist list
                    )
                )
                unchangedLists
                |> Debug.log "unchangedTodolists"
    in
        ( (List.map ((flip (,)) False) oldLists__)
            ++ newLists__
            ++ unchangedLists_
            |> List.foldl
                (\( list, isNew ) lists ->
                    if isNew then
                        list :: lists
                    else
                        replace list.uri list lists
                )
                listOfLists
        , cmds
            |> Cmd.batch
        )


string2ValueList : String -> String -> List (LSEQ.Entry Char)
string2ValueList origin str =
    String.toList str
        |> List.map (LSEQ.Single origin << LSEQ.Value)


isValue : LSEQ.Value a -> Bool
isValue v =
    case v of
        LSEQ.Value _ ->
            True

        LSEQ.Tomb _ ->
            False


isCementary : LSEQ.Entry a -> Bool
isCementary entry =
    case entry of
        LSEQ.MVR mvr ->
            Dict.values mvr |> List.any isValue |> not

        LSEQ.Single _ (LSEQ.Tomb _) ->
            True

        _ ->
            False


diffString : Uri -> BString -> String -> ( BString, List (Local Char) )
diffString namespace old new =
    let
        oldString =
            bStringToString namespace old
                |> Debug.log "old"

        diff =
            Diff.diff (String.toList oldString) (String.toList new)
                |> Debug.log "diff"

        foldChange change ( newStr, oldvalues, operations, i ) =
            let
                added c =
                    ( newStr ++ [ LSEQ.Single namespace (LSEQ.Value c) ]
                    , oldvalues
                    , operations ++ [ ( ( namespace, i ), LSEQ.Insert c ) ]
                    , i + 1
                    )
            in
                case oldvalues of
                    oldvalue :: rest ->
                        if isCementary oldvalue then
                            foldChange change ( newStr, rest, operations, i )
                        else
                            case change of
                                Diff.Removed char ->
                                    let
                                        ( newStr_, localOp, i_ ) =
                                            case oldvalue of
                                                LSEQ.Single target (LSEQ.Value v) ->
                                                    ( newStr, Just ( ( target, i ), LSEQ.Remove ), i )

                                                LSEQ.Single target (LSEQ.Tomb v) ->
                                                    -- cannot happen, since already filtered by isCementary
                                                    ( newStr, Nothing, i )

                                                LSEQ.MVR mvr ->
                                                    case Dict.get namespace mvr of
                                                        Just (LSEQ.Value v) ->
                                                            let
                                                                mvr_ =
                                                                    Dict.insert namespace (LSEQ.Tomb v) mvr
                                                            in
                                                                ( newStr ++ [ LSEQ.MVR mvr_ ]
                                                                , Just ( ( namespace, i ), LSEQ.Remove )
                                                                , i
                                                                    + (if Dict.values mvr_ |> List.any isValue then
                                                                        1
                                                                       else
                                                                        0
                                                                      )
                                                                )

                                                        _ ->
                                                            (Dict.toList mvr
                                                                |> List.filterMap
                                                                    (\( k, v ) ->
                                                                        case v of
                                                                            LSEQ.Tomb _ ->
                                                                                Nothing

                                                                            LSEQ.Value v ->
                                                                                Just ( k, v )
                                                                    )
                                                                |> List.sortBy first
                                                                |> List.head
                                                                |> Maybe.map
                                                                    (\( target, v ) ->
                                                                        let
                                                                            mvr_ =
                                                                                Dict.insert target (LSEQ.Tomb v) mvr
                                                                        in
                                                                            ( newStr ++ [ LSEQ.MVR mvr_ ]
                                                                            , Just ( ( target, i ), LSEQ.Remove )
                                                                            , i
                                                                                + (if Dict.values mvr_ |> List.any isValue then
                                                                                    1
                                                                                   else
                                                                                    0
                                                                                  )
                                                                            )
                                                                    )
                                                                |> Maybe.withDefault ( newStr, Nothing, i )
                                                            )

                                        operations_ =
                                            Maybe.map (\l -> operations ++ [ l ]) localOp
                                                |> Maybe.withDefault operations
                                    in
                                        ( newStr_, rest, operations_, i_ )

                                Diff.Added c ->
                                    added c

                                Diff.NoChange _ ->
                                    ( newStr ++ [ oldvalue ], rest, operations, i + 1 )

                    [] ->
                        case change of
                            Diff.Added c ->
                                added c

                            _ ->
                                ( newStr, [], operations, i )

        ( newStr, _, operations, _ ) =
            List.foldl foldChange ( [], old, [], 0 ) diff
    in
        ( newStr, operations )


main =
    Html.programWithFlags
        { init = Bright.Model.init
        , update = Bright.Update.update
        , subscriptions = Bright.Sub.subscriptions
        , view = Bright.View.view
        }
