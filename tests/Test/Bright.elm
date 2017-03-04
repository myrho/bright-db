module Test.Bright exposing (..)

import Tuple exposing (first, second)
import Test exposing (..)
import Expect
import Bright exposing (charOperation)
import Bright.Model exposing (..)
import String


type alias Data =
    { current : String
    , new : String
    , result : List ( Op, Int )
    }


data =
    [ Data "word" "word" []
    , Data "word" "2word" [ ( Insert (Char '2'), 0 ) ]
    , Data "word" "w2ord" [ ( Insert (Char '2'), 1 ) ]
    , Data "word" "wo2rd" [ ( Insert (Char '2'), 2 ) ]
    , Data "word" "wor2d" [ ( Insert (Char '2'), 3 ) ]
    , Data "word" "word2" [ ( Insert (Char '2'), 4 ) ]
    , Data "word" "word 2" [ ( Insert (Char ' '), 4 ) ]
    , Data "word" "wword" [ ( Insert (Char 'w'), 1 ) ]
    , Data "2word" "word" [ ( Remove, 0 ) ]
    , Data "w2ord" "word" [ ( Remove, 1 ) ]
    , Data "wo2rd" "word" [ ( Remove, 2 ) ]
    , Data "wor2d" "word" [ ( Remove, 3 ) ]
    , Data "word2" "word" [ ( Remove, 4 ) ]
    , Data "word 2" "word" [ ( Remove, 4 ) ]
    ]


testCharOperation =
    describe "CharOperation" <|
        List.map
            (\{ current, new, result } ->
                test ("charOperation from " ++ current ++ " to " ++ new) <|
                    \() ->
                        Expect.equal
                            (charOperation (String.toList current) (String.toList new))
                            result
            )
            data


all : Test
all =
    describe "Bright API" <|
        [ testCharOperation
        ]
