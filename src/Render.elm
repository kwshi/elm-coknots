module Render exposing (..)

import Coknot.Layout as Layout
import Coknot.Orient as Orient
import Coknot.Route as Route
import Dict
import Html.Styled as Html
import Html.Styled.Attributes as Hat
import Svg.PathD as Path
import Svg.Styled as Svg
import Svg.Styled.Attributes as At


strokeAttrs : List (Svg.Attribute msg)
strokeAttrs =
    [ At.stroke "black"
    , Hat.attribute "vector-effect" "non-scaling-stroke"
    , At.fill "transparent"
    ]


semicirc :
    Layout.Side
    -> ( Float, List Path.Segment )
    -> ( Float, List Path.Segment )
    -> List Path.Segment
semicirc side ( x1, p1 ) ( x2, p2 ) =
    let
        r =
            (x2 - x1) / 2

        offset =
            case side of
                Layout.N ->
                    -0.125

                Layout.S ->
                    0.125
    in
    p1
        ++ [ Path.M ( x1, 0 )
           , Path.L ( x1, offset )
           , Path.A ( r, r )
                0
                True
                (case side of
                    Layout.N ->
                        True

                    Layout.S ->
                        False
                )
                ( x2, offset )
           , Path.L ( x2, 0 )
           ]
        ++ p2


perturb : Layout.Endpoint -> ( Float, List Path.Segment )
perturb { dir, x } =
    case dir of
        Layout.W ->
            ( toFloat x - 0.25
            , [ Path.M ( toFloat x, 0 )
              , Path.L ( toFloat x - 0.25, 0 )
              ]
            )

        Layout.E ->
            ( toFloat x + 0.25
            , [ Path.M ( toFloat x, 0 )
              , Path.L ( toFloat x + 0.25, 0 )
              ]
            )

        Layout.V ->
            ( toFloat x
            , []
            )


arcPath : Layout.Side -> Layout.Endpoint -> Layout.Endpoint -> List Path.Segment
arcPath side start end =
    if end.x - start.x == 1 && start.dir == Layout.E && end.dir == Layout.W then
        [ Path.M ( toFloat start.x, 0 )
        , Path.L ( toFloat end.x, 0 )
        ]

    else
        semicirc side (perturb start) (perturb end)


render : Route.Success -> Html.Html msg
render { layout, width } =
    let
        _ =
            Debug.log "width" width
    in
    Svg.svg
        [ At.width "100%"
        , At.height "100%"
        , At.viewBox <|
            "-1 "
                ++ String.fromFloat -(toFloat (layout.largest + 1) / 2)
                ++ " "
                ++ String.fromInt (width + 1)
                ++ " "
                ++ String.fromInt (layout.largest + 1)
        ]
        [ Svg.g []
            (layout.strokes
                |> Dict.values
                |> List.concat
                |> List.map
                    (\{ side, start, end } ->
                        Svg.path
                            ((At.d << Path.pathD <| arcPath side start end) :: strokeAttrs)
                            []
                    )
            )
        , Svg.g []
            (layout.crossings
                |> Dict.toList
                |> List.concatMap
                    (\( x, t ) ->
                        [ Svg.circle
                            [ At.cx <| String.fromInt x
                            , At.cy "0"
                            , At.r "0.125"
                            , At.fill "#f7fafc"
                            ]
                            []
                        , case t.dominant of
                            Orient.H ->
                                Svg.line
                                    ([ At.x1 <| String.fromFloat (toFloat x - 0.125)
                                     , At.x2 <| String.fromFloat (toFloat x + 0.125)
                                     , At.y1 "0"
                                     , At.y2 "0"
                                     ]
                                        ++ strokeAttrs
                                    )
                                    []

                            Orient.V ->
                                Svg.line
                                    ([ At.x1 <| String.fromInt x
                                     , At.x2 <| String.fromInt x
                                     , At.y1 "-0.125"
                                     , At.y2 ".125"
                                     ]
                                        ++ strokeAttrs
                                    )
                                    []
                        ]
                    )
            )
        ]
