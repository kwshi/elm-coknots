module Style exposing (..)

import Css exposing (..)
import Css.Systems.Colors as Cl
import Css.Systems.Spacing as Sp
import Css.Systems.Text as Tx
import Css.Systems.Utilities as Ut


root : Style
root =
    [ minWidth Sp.sizeLG
    , position absolute
    , top Sp.space0
    , left Sp.space0
    , bottom Sp.space0
    , right Sp.space0
    ]
        |> batch


content : Style
content =
    [ Ut.marginX auto
    , minWidth Sp.sizeSM
    , maxWidth Sp.size4XL
    , padding2 Sp.space2 Sp.space32
    , displayFlex
    , flexDirection column
    ]
        |> batch


input : Style
input =
    [ border3 (px 1) solid Cl.gray400
    , padding2 Sp.space2 Sp.space3
    , fontFamilies [ "Roboto Mono", "monospace" ]
    , fontSize inherit
    ]
        |> batch
