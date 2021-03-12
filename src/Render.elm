module Render exposing (..)

import Coknot.Layout as Layout
import Coknot.Orient as Orient
import Coknot.Postprocess as Post
import Coknot.Route as Route
import Css
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


padSize : Float
padSize =
    0.5


roundSize : Float
roundSize =
    1 / 8


sideSign : Layout.Side -> number
sideSign side =
    case side of
        Layout.N ->
            -1

        Layout.S ->
            1


semicirc :
    Layout.Side
    -> ( Float, List Path.Segment )
    -> ( Float, List Path.Segment )
    -> List Path.Segment
semicirc side ( x1, p1 ) ( x2, p2 ) =
    let
        r =
            (x2 - x1) / 2

        roundOffset =
            case side of
                Layout.N ->
                    -roundSize

                Layout.S ->
                    roundSize

        offset =
            case side of
                Layout.N ->
                    -padSize

                Layout.S ->
                    padSize
    in
    p1
        ++ [ Path.M ( x1, roundOffset )
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
           , Path.L ( x2, roundOffset )
           ]
        ++ p2


round : Layout.Side -> Layout.Dir -> Int -> Path.Segment
round side dir x =
    let
        dy =
            sideSign side * roundSize

        sweep =
            (dir == Layout.E && side == Layout.S)
                || (dir == Layout.W && side == Layout.N)
    in
    Path.A ( roundSize, roundSize )
        90
        False
        sweep
        ( toFloat x, dy )


perturb : Layout.Side -> Layout.Endpoint -> ( Float, List Path.Segment )
perturb side { dir, x } =
    let
        rd =
            round side dir x
    in
    case dir of
        Layout.W ->
            ( toFloat x
            , [ Path.M ( toFloat x + 1 - padSize, 0 )
              , Path.L ( toFloat x + roundSize, 0 )
              , rd
              ]
            )

        Layout.E ->
            ( toFloat x
            , [ Path.M ( toFloat x - 1, 0 )
              , Path.L ( toFloat x - roundSize, 0 )
              , rd
              ]
            )

        Layout.V ->
            ( toFloat x
            , [ Path.M ( toFloat x, 0 )
              , Path.L ( toFloat x, sideSign side * roundSize )
              ]
            )


arcPath :
    Bool
    -> Layout.Side
    -> Layout.Endpoint
    -> Layout.Endpoint
    -> List Path.Segment
arcPath flat side start end =
    if
        flat
            && (end.x - start.x == 1)
            && (start.dir == Layout.E)
            && (end.dir == Layout.W)
    then
        [ Path.M ( toFloat start.x - 1, 0 )
        , Path.L ( toFloat end.x + 1, 0 )
        ]

    else
        semicirc side (perturb side start) (perturb side end)


render : Bool -> Route.Success -> Html.Html msg
render flat { layout, width } =
    let
        _ =
            Debug.log "width" width

        height =
            toFloat layout.largest + 3 * padSize

        posted =
            Post.process { skipTrivial = True } layout
                |> Debug.log "posted"
    in
    Svg.svg
        [ At.width "100%"
        , At.height "100%"
        , At.viewBox <|
            "-1 "
                ++ String.fromFloat -(height / 2)
                ++ " "
                ++ String.fromInt (width + 1)
                ++ " "
                ++ String.fromFloat height
        ]
        [ Svg.g []
            (posted.arcs
                |> List.concatMap .stroke
                |> List.map
                    (\{ side, start, end } ->
                        Svg.path
                            ([ At.d <| Path.pathD <| arcPath flat side start end ]
                                ++ strokeAttrs
                            )
                            []
                    )
            )
        , Svg.g
            []
            (posted.crossings
                |> List.map (\{ x, terminal } -> ( x, terminal ))
                |> List.concatMap
                    (\( x, t ) ->
                        [ Svg.circle
                            [ At.cx <| String.fromInt x
                            , At.cy "0"
                            , At.r <| String.fromFloat padSize
                            , At.fill "#f7fafc"
                            ]
                            []
                        , case t.dominant of
                            Orient.H ->
                                Svg.line
                                    ([ At.x1 <| String.fromFloat (toFloat x - padSize)
                                     , At.x2 <| String.fromFloat (toFloat x + padSize)
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
                                     , At.y1 <| String.fromFloat -padSize
                                     , At.y2 <| String.fromFloat padSize
                                     ]
                                        ++ strokeAttrs
                                    )
                                    []
                        ]
                    )
            )
        ]
