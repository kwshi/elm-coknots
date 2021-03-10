module Preset exposing (..)

import ElmEscapeHtml as Esc
import Html.Styled as Ht


type alias Example msg =
    { name : List (Ht.Html msg), code : String }


all : List (Example msg)
all =
    [ Example [ Ht.text "Unknot" ] ""
    , Example [ Ht.text "Trefoil", Ht.sup [] [ Ht.text "+" ] ]
        "1o+ 2u+ 3o+ 1u+ 2o+ 3u+"
    , Example
        [ Ht.text "Trefoil"
        , Ht.sup [] [ Ht.text <| Esc.unescape "&minus;" ]
        ]
        "1o- 2u- 3o- 1u- 2o- 3u-"
    , Example
        [ Ht.text "(10,45)" ]
        "1u- 2o+ 3u+ 1o- 4u- 5o+ 6u+ 7o- 8u- 3o+ 2u+ 4o- 9u- 6o+ 10u+ 8o- 7u- 10o+ 5u+ 9o-"
    , Example
        [ Ht.text "(10,86)" ]
        "1u- 2o- 3u+ 4o+ 5u- 1o- 2u- 6o- 4u+ 7o+ 8u+ 9o+ 10u+ 3o+ 6u- 5o- 7u+ 10o+ 9u+ 8o+"
    ]
