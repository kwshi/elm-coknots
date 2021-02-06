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
    ]
