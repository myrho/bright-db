module Test.RemoteUpdate exposing (..)

import Tuple exposing (first, second)
import Test exposing (..)
import Expect
import Dict
import PlainArray as Array
import Bright.Update exposing (remoteOperationsUpdate, operations2Body)
import Bright.Model exposing (..)
import String


predicate =
    "predicate"


localOperations =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        ]


localOperations2 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Just <| Operation "ns1" predicate 3 <| Insert <| Char 'd'
        ]


op1 =
    ( 3, Operation "ns1" predicate 3 <| Insert <| Char 'd' )


operations1 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Just <| second op1
        ]


op2 =
    ( 3, Operation "ns1" predicate 0 <| Remove )


operations2 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Just <| second op2
        ]


op3_in =
    Operation "ns2" predicate 0 <| Insert <| Char 'x'


op3_out =
    Operation "ns2" predicate 0 <| Insert <| Char 'x'


op31 =
    ( 0, op3_in )


operations31 =
    Array.fromList
        [ Just <| op3_out
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 3 <| Insert <| Char 'c'
        ]


op32 =
    ( 1, op3_in )


operations32 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| op3_out
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 3 <| Insert <| Char 'c'
        ]


op33 =
    ( 2, op3_in )


operations33 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| op3_out
        , Just <| Operation "ns1" predicate 3 <| Insert <| Char 'c'
        ]


op34_1 =
    ( 2, Operation "ns2" predicate 1 <| Insert <| Char 'x' )


op34_2 =
    ( 3, Operation "ns2" predicate 2 <| Insert <| Char 'y' )


operations34 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns2" predicate 1 <| Insert <| Char 'x'
        , Just <| Operation "ns2" predicate 2 <| Insert <| Char 'y'
        , Just <| Operation "ns1" predicate 4 <| Insert <| Char 'c'
        , Just <| Operation "ns1" predicate 5 <| Insert <| Char 'd'
        ]


localOperations341 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns2" predicate 2 <| Insert <| Char 'c'
        , Just <| Operation "ns2" predicate 3 <| Insert <| Char 'd'
        ]


op341_1 =
    ( 2, Operation "ns1" predicate 1 <| Insert <| Char 'x' )


op341_2 =
    ( 3, Operation "ns1" predicate 2 <| Insert <| Char 'y' )


{-|
  folded to axybcd
-}
operations341 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns2" predicate 2 <| Insert <| Char 'c'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'x'
        , Just <| Operation "ns2" predicate 4 <| Insert <| Char 'd'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'y'
        ]



{-
   [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
   , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
   , Just <| Operation "ns2" predicate 2 <| Insert <| Char 'c'
   , Just <| Operation "ns2" predicate 3 <| Insert <| Char 'd'
   , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'x'
   , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'y'
   ]
-}


localOperations35 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'x'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'y'
        ]


{-| test 341 and 35 should result in the same history
it's the case of two peers syncing their states vice-versa
-}
op35_1 =
    ( 2, Operation "ns2" predicate 2 <| Insert <| Char 'c' )


op35_2 =
    ( 3, Operation "ns2" predicate 3 <| Insert <| Char 'd' )


{-|
  folded to axybcd, but with the wrong history!
  TODO: how to adapt the sync algo that the same history as 341 results?
-}
operations35 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns2" predicate 2 <| Insert <| Char 'c'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'x'
        , Just <| Operation "ns2" predicate 3 <| Insert <| Char 'd'
        , Just <| Operation "ns1" predicate 4 <| Insert <| Char 'y'
        ]



{-
   [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
   , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
   , Just <| Operation "ns2" predicate 2 <| Insert <| Char 'c'
   , Just <| Operation "ns2" predicate 3 <| Insert <| Char 'd'
   , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'x'
   , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'y'
   ]
-}


op4 =
    ( 1, Operation "ns2" predicate 0 <| Remove )


operations4 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns2" predicate 0 <| Remove
        , Just <| Operation "ns1" predicate 0 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'c'
        ]


op5 =
    ( 5, Operation "ns2" predicate 0 <| Remove )


operations5 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Nothing
        , Nothing
        , Just <| second op5
        ]


op51 =
    ( 4, Operation "ns2" predicate 0 <| Remove )


operations51 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Nothing
        , Just <| second op51
        , Just <| second op5
        ]


operations52 =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Just <| second op51
        , Just <| second op51
        , Just <| second op5
        ]


op3r_in =
    Operation "ns0" predicate 0 <| Insert <| Char 'x'


op3r_out =
    Operation "ns0" predicate 1 <| Insert <| Char 'x'


op31r =
    ( 0, op3r_in )


operations31r =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns0" predicate 0 <| Insert <| Char 'x'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 3 <| Insert <| Char 'c'
        ]


op32r =
    ( 1, op3r_in )


operations32r =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns0" predicate 0 <| Insert <| Char 'x'
        , Just <| Operation "ns1" predicate 3 <| Insert <| Char 'c'
        ]


op33r =
    ( 2, op3r_in )


operations33r =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Just <| Operation "ns0" predicate 0 <| Insert <| Char 'x'
        ]


op34r_1 =
    ( 2, Operation "ns0" predicate 1 <| Insert <| Char 'x' )


op34r_2 =
    ( 3, Operation "ns0" predicate 2 <| Insert <| Char 'y' )


operations34r =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Just <| Operation "ns0" predicate 1 <| Insert <| Char 'x'
        , Just <| Operation "ns0" predicate 2 <| Insert <| Char 'y'
        , Just <| Operation "ns1" predicate 5 <| Insert <| Char 'd'
        ]


op4r =
    ( 1, Operation "ns0" predicate 0 <| Remove )


operations4r =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns0" predicate 0 <| Remove
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'c'
        ]


op5r =
    ( 5, Operation "ns0" predicate 0 <| Remove )


operations5r =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Nothing
        , Nothing
        , Just <| second op5r
        ]


op51r =
    ( 4, Operation "ns0" predicate 0 <| Remove )


operations51r =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Nothing
        , Just <| second op51r
        , Just <| second op5r
        ]


operations52r =
    Array.fromList
        [ Just <| Operation "ns1" predicate 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" predicate 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" predicate 2 <| Insert <| Char 'c'
        , Just <| second op51r
        , Just <| second op51r
        , Just <| second op5r
        ]


operations6 =
    Array.fromList
        [ Just <| Operation "ns1" "p1" 0 <| Insert <| Char 'a'
        , Just <| Operation "ns1" "p1" 1 <| Insert <| Char 'b'
        , Just <| Operation "ns1" "p2" 0 <| Insert <| Char 'c'
        , Just <| Operation "ns1" "p2" 1 <| Insert <| Char 'd'
        , Just <| Operation "ns1" "p2" 0 <| Remove
        ]


data =
    [ ( "op1", op1, operations1, localOperations, 1 )
    , ( "op2", op2, operations2, localOperations )
    , ( "op31", op31, operations31, localOperations )
    , ( "op32", op32, operations32, localOperations )
    , ( "op33", op33, operations33, localOperations )
    , ( "op4", op4, operations4, localOperations )
    , ( "op5", op5, operations5, localOperations )
    ]


data2 =
    [ ( "op31r", op31r, operations31r, localOperations )
    , ( "op32r", op32r, operations32r, localOperations )
    , ( "op33r", op33r, operations33r, localOperations )
    , ( "op4r", op4r, operations4r, localOperations )
    , ( "op5r", op5r, operations5r, localOperations )
    , ( "op51r", op51r, operations51r, operations5r )
    ]


data3 =
    [ ( "op34", [ op34_1, op34_2 ], operations34, localOperations2, 2 )
    , ( "op341", [ op341_1, op341_2 ], operations34, localOperations341, 2 )
    , ( "op35", [ op35_1, op35_2 ], operations35, localOperations35, 2 )
    , ( "op34r", [ op34r_1, op34r_2 ], operations34r, localOperations2, 2 )
    ]


words =
    [ ( "op1", operations1, [ ( predicate, Literal <| Array.fromList <| String.toList "abcd" ) ] )
    , ( "op2", operations2, [ ( predicate, Literal <| Array.fromList <| String.toList "bc" ) ] )
    , ( "op4", operations4, [ ( predicate, Literal <| Array.fromList <| String.toList "bc" ) ] )
    , ( "op5", operations5, [ ( predicate, Literal <| Array.fromList <| String.toList "bc" ) ] )
    , ( "op51", operations51, [ ( predicate, Literal <| Array.fromList <| String.toList "c" ) ] )
    , ( "op52", operations52, [] )
    , ( "op6"
      , operations6
      , [ ( "p1", Literal <| Array.fromList [ 'a', 'b' ] )
        , ( "p2", Literal <| Array.fromList [ 'd' ] )
        ]
      )
    ]


remoteUpdate name data =
    describe name <|
        List.map
            (\( name, op, operations, local, finalStateDelta ) ->
                test ("updatePast " ++ name) <|
                    \() ->
                        Expect.equal
                            (remoteOperationsUpdate 0 op local)
                            ( finalStateDelta, operations )
            )
            data


remoteUpdate2 name data =
    describe name <|
        List.map
            (\( name, ops, operations, local, finalStateDelta ) ->
                test ("updatePast2 " ++ name) <|
                    \() ->
                        Expect.equal
                            (List.foldl
                                (\op ( s, local ) ->
                                    remoteOperationsUpdate s op local
                                )
                                ( 0, local )
                                ops
                            )
                            ( finalStateDelta, operations )
            )
            data


all : Test
all =
    describe "Update" <|
        [ remoteUpdate "Remote Update" data
        , remoteUpdate "Remote Update R" data2
        , remoteUpdate2 "Remote Update R2" data3
        , describe "Folded" <|
            List.map
                (\( name, operations, result ) ->
                    test ("foldOperation " ++ name) <|
                        \() ->
                            Expect.equal
                                (operations2Body operations
                                    |> Dict.toList
                                )
                                result
                )
                words
        ]
