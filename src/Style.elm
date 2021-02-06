module Style exposing (..)

import Css exposing (..)
import Css.Systems.Colors as Cl
import Css.Systems.Spacing as Sp
import Css.Systems.Text as Tx
import Css.Systems.Utilities as Ut


transitionBg : Style
transitionBg =
    property "transition" "50ms background-color linear"


root : Style
root =
    [ minWidth Sp.sizeLG
    , position absolute
    , top Sp.space0
    , left Sp.space0
    , bottom Sp.space0
    , right Sp.space0
    , backgroundColor Cl.gray100
    , color Cl.gray900
    , Tx.textSM
    , fontFamilies [ "Inter", "sans-serif" ]
    ]
        |> batch


content : Style
content =
    [ Ut.marginX auto
    , minWidth Sp.sizeSM
    , maxWidth Sp.size4XL
    , Ut.paddingX Sp.space32
    , displayFlex
    , flexDirection column
    ]
        |> batch


controls : Style
controls =
    [ displayFlex
    , flexDirection column
    , Ut.paddingY Sp.space4
    , borderBottom3 (px 1) solid Cl.gray300
    , marginBottom Sp.space4
    ]
        |> batch


code : Style
code =
    [ fontSize inherit
    , fontFamilies [ "Roboto Mono", "monospace" ]
    , backgroundColor Cl.blue100
    , Ut.paddingX Sp.space1
    , borderRadius Sp.space1
    , border3 (px 1) solid Cl.blue200
    ]
        |> batch


examples : Style
examples =
    [ displayFlex
    , Ut.itemsBaseline
    ]
        |> batch


examplesLabel : Style
examplesLabel =
    [ fontWeight bold
    , marginRight Sp.space2
    ]
        |> batch


example : Style
example =
    [ border Sp.space0
    , cursor pointer
    , color Cl.blue600
    , backgroundColor transparent
    , margin Sp.space0
    , transitionBg
    , padding2 Sp.space1 Sp.space2
    , fontFamily inherit
    , fontSize inherit
    , hover
        [ backgroundColor Cl.blue100
        ]
    ]
        |> batch


input : Style
input =
    [ border3 (px 1) solid Cl.blue300
    , height Sp.space4
    , lineHeight Sp.space4
    , padding2 Sp.space2 Sp.space3
    , resize none
    , whiteSpace pre
    , color inherit
    , overflowY hidden
    , property "scrollbar-width" "none"
    , fontFamilies [ "Roboto Mono", "monospace" ]
    , fontSize inherit
    , margin Sp.space0
    ]
        |> batch


carets : Style
carets =
    [ whiteSpace pre
    , Ut.paddingX Sp.space3
    , height Sp.space6
    , color Cl.red800
    , fontWeight bold
    , fontFamilies [ "Roboto Mono", "monospace" ]
    , borderLeft3 (px 1) solid transparent
    , borderRight3 (px 1) solid transparent
    , cursor default
    ]
        |> batch


highlight : Style
highlight =
    [ backgroundColor Cl.red200
    ]
        |> batch


caret : Style
caret =
    [ paddingTop Sp.space1
    , display inlineBlock
    , transitionBg
    ]
        |> batch


errs : Style
errs =
    [ margin Sp.space0
    , property "display" "grid"
    , property "grid-template-columns" "max-content 1fr"
    ]
        |> batch


errLabel : Style
errLabel =
    [ fontWeight bold
    , color Cl.red800
    , paddingLeft Sp.space3
    , paddingRight Sp.space2
    , Ut.paddingY Sp.space1
    , transitionBg
    , after
        [ property "content" "\":\""
        ]
    ]
        |> batch


errMsg : Style
errMsg =
    [ margin Sp.space0
    , Ut.paddingY Sp.space1
    , Ut.flexInitial
    , transitionBg
    ]
        |> batch
