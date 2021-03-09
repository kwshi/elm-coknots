module Render exposing (..)

import Coknot.Layout as Layout
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


semicirc : Layout.Side -> Float -> Float -> List Path.Segment
semicirc side x1 x2 =
    let
        r =
            (x2 - x1) / 2
    in
    [ Path.M ( x1, 0 )
    , Path.A ( r, r )
        0
        True
        (case side of
            Layout.N ->
                True

            Layout.S ->
                False
        )
        ( x2, 0 )
    ]


perturb : Layout.Endpoint -> Float
perturb { dir, x } =
    case dir of
        Layout.W ->
            toFloat x - 0.2

        Layout.E ->
            toFloat x + 0.2

        Layout.V ->
            toFloat x


arcPath : Layout.Side -> Layout.Endpoint -> Layout.Endpoint -> List Path.Segment
arcPath side start end =
    if end.x - start.x == 1 && start.dir == Layout.E && end.dir == Layout.W then
        [ Path.M ( toFloat start.x + 0.1, 0 ), Path.L ( toFloat end.x - 0.1, 0 ) ]

    else
        semicirc side (perturb start) (perturb end)


render : Route.Success -> Html.Html msg
render { layout, width } =
    let
        _ =
            Debug.log "width" width
    in
    layout.strokes
        |> Dict.values
        |> List.concat
        |> List.map
            (\{ side, start, end } ->
                Svg.path
                    ((At.d << Path.pathD <| arcPath side start end) :: strokeAttrs)
                    []
            )
        |> Svg.svg
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
