module Render exposing (..)

import Dict
import Html.Styled as Html
import Html.Styled.Attributes as Hat
import Layout
import Svg.PathD as Path
import Svg.Styled as Svg
import Svg.Styled.Attributes as At


strokeAttrs : List (Svg.Attribute msg)
strokeAttrs =
    [ At.stroke "black"
    , Hat.attribute "vector-effect" "non-scaling-stroke"
    , At.fill "transparent"
    ]


render : Layout.Layout -> Html.Html msg
render =
    Dict.values
        >> List.concat
        >> List.map
            (\part ->
                case part of
                    Layout.Spine x1 x2 ->
                        Svg.path
                            (([ Path.M ( toFloat x1, 0 )
                              , Path.L ( toFloat x2, 0 )
                              ]
                                |> (Path.pathD >> At.d)
                             )
                                :: strokeAttrs
                            )
                            []

                    Layout.Arc side x1 x2 ->
                        let
                            r =
                                toFloat (x2 - x1) / 2
                        in
                        Svg.path
                            (([ Path.M ( toFloat x1, 0 )
                              , Path.A ( r, r )
                                    0
                                    True
                                    (case side of
                                        Layout.N ->
                                            True

                                        Layout.S ->
                                            False
                                    )
                                    ( toFloat x2, 0 )
                              ]
                                |> (Path.pathD >> At.d)
                             )
                                :: strokeAttrs
                            )
                            []
            )
        >> Svg.svg
            [ At.width "300"
            , At.height "300"
            , At.viewBox "-4 -4 8 8"
            ]
