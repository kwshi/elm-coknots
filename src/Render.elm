module Render exposing (..)

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


semicirc : Route.Side -> Float -> Float -> List Path.Segment
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
            Route.N ->
                True

            Route.S ->
                False
        )
        ( x2, 0 )
    ]


perturb : Route.Endpoint -> Float
perturb { dir, x } =
    case dir of
        Route.W ->
            toFloat x - 0.2

        Route.E ->
            toFloat x + 0.2

        Route.V ->
            toFloat x


arcPath : Route.Side -> Route.Endpoint -> Route.Endpoint -> List Path.Segment
arcPath side start end =
    if end.x - start.x == 1 && start.dir == Route.E && end.dir == Route.W then
        [ Path.M ( toFloat start.x + 0.1, 0 ), Path.L ( toFloat end.x - 0.1, 0 ) ]

    else
        semicirc side (perturb start) (perturb end)


render : Route.Success -> Html.Html msg
render { layout, width } =
    let
        _ =
            Debug.log "width" width
    in
    layout
        |> Dict.values
        |> List.concat
        |> List.map
            (\part ->
                case part of
                    Route.Spine x1 x2 ->
                        Svg.path
                            (([ Path.M ( toFloat x1, 0 )
                              , Path.L ( toFloat x2, 0 )
                              ]
                                |> (Path.pathD >> At.d)
                             )
                                :: strokeAttrs
                            )
                            []

                    Route.Arc side x1 x2 ->
                        let
                            r =
                                toFloat (x2.x - x1.x) / 2
                        in
                        Svg.path
                            ((arcPath side x1 x2
                                |> (Path.pathD >> At.d)
                             )
                                :: strokeAttrs
                            )
                            []
            )
        |> Svg.svg
            [ At.width "100%"
            , At.height "400"
            , At.viewBox <| "-1 -12 " ++ String.fromInt (width + 1) ++ " 25"
            ]
