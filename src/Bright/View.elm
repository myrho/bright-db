module Bright.View exposing (..)

import Html exposing (text, ul, li, div, h3, h2)
import Html.Attributes exposing (class)
import Set


view model =
    div
        [ class "connection" ]
        [ h2 [] [ text "Connection status" ]
        , h3 [] [ text <| "You are on " ++ model.namespace ]
        , h3 [] [ text "Online peer devices:" ]
        , peers model
        ]


peers model =
    if Set.isEmpty model.peers then
        div
            [ class "note" ]
            [ text "Open this page on other devices too to get them connected!"
            , text " (you might need to refresh the page)"
            ]
    else
        ul
            []
        <|
            List.map
                (\peer ->
                    li
                        []
                        [ text peer
                        ]
                )
            <|
                Set.toList model.peers
