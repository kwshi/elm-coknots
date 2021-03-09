module Coknot.Route exposing (..)

import Coknot.Orient as Orient
import Dict
import Set
import Stact


type Side
    = N
    | S


type Dir
    = V
    | W
    | E


type alias Endpoint =
    { dir : Dir
    , x : Int
    }


type Part
    = Arc Side Endpoint Endpoint
    | Spine Int Int


type alias Stroke =
    List Part


type alias Layout =
    Dict.Dict Int Stroke


type alias State =
    { layout : Layout
    , first : Orient.Terminal
    , above : Stact.Stact Seg Endpoint
    , below : Stact.Stact Seg Endpoint
    , x : Int
    }


type alias Seg =
    Int


type alias Success =
    { layout : Layout
    , width : Int
    }


init : Orient.Terminal -> State
init first =
    { x = 0
    , first = first
    , above = Stact.empty
    , below = Stact.empty
    , layout = Dict.empty
    }


initPush : a -> Maybe (List a) -> Maybe (List a)
initPush a =
    Maybe.withDefault [] >> (::) a >> Just


type alias ShiftState =
    { x : Int
    , from : Stact.Stact Seg Endpoint
    , to : Stact.Stact Seg Endpoint
    , layout : Layout
    }


shift : ( Int, Side, Side ) -> ShiftState -> Maybe ShiftState
shift ( seg, fromSide, toSide ) state =
    Stact.popUntil seg
        (\s start { x, layout, to } ->
            to
                |> Stact.pushOrPop s { x = x, dir = V }
                |> (\( popped, newTo ) ->
                        { x = x + 1
                        , to = newTo
                        , layout =
                            Dict.update s
                                (update <| (::) <| Arc fromSide start { x = x, dir = V })
                                layout
                                |> (case popped of
                                        Nothing ->
                                            identity

                                        Just otherStart ->
                                            Dict.update s <|
                                                update <|
                                                    (::) <|
                                                        Arc toSide otherStart { x = x, dir = V }
                                   )
                        }
                   )
        )
        { x = state.x
        , to = state.to
        , layout = state.layout
        }
        state.from
        |> Maybe.map
            (\( start, { to, layout, x }, from ) ->
                { state
                    | x = x
                    , to = to
                    , from = from
                    , layout =
                        Dict.update seg
                            (update <|
                                (::) (Arc fromSide start { x = x, dir = W })
                            )
                            layout
                }
            )


west : Int -> State -> Maybe State
west seg state =
    if Stact.member seg state.above then
        { x = state.x
        , from = state.above
        , to = state.below
        , layout = state.layout
        }
            |> shift ( seg, N, S )
            |> Maybe.map
                (\{ x, from, to, layout } ->
                    { state
                        | x = x
                        , above = from
                        , below = to
                        , layout = layout
                    }
                )

    else if Stact.member seg state.below then
        { x = state.x
        , from = state.below
        , to = state.above
        , layout = state.layout
        }
            |> shift ( seg, S, N )
            |> Maybe.map
                (\{ x, from, to, layout } ->
                    { state
                        | x = x
                        , below = from
                        , above = to
                        , layout = layout
                    }
                )

    else if state.first.w == seg then
        -- TODO handle first better
        { state
            | above = Stact.push seg { x = state.x, dir = W } state.above
            , below = Stact.push seg { x = state.x, dir = W } state.below
            , layout = state.layout
        }
            |> Just

    else
        Just state


north : Int -> State -> State
north seg state =
    { seg = seg
    , stact = state.above
    , side = N
    , layout = state.layout
    , x = state.x
    }
        |> vert
        |> (\{ stact, layout } ->
                { state | above = stact, layout = layout }
           )


incX : State -> State
incX state =
    { state | x = state.x + 1 }


south : Int -> State -> State
south seg state =
    { seg = seg
    , stact = state.below
    , side = S
    , x = state.x
    , layout = state.layout
    }
        |> vert
        |> (\{ stact, layout } ->
                { state | below = stact, layout = layout }
           )


vert :
    { seg : Int
    , stact : Stact.Stact Seg Endpoint
    , side : Side
    , x : Int
    , layout : Layout
    }
    ->
        { stact : Stact.Stact Seg Endpoint
        , layout : Layout
        }
vert { seg, stact, side, x, layout } =
    Stact.pushOrPop seg { x = x, dir = V } stact
        |> (\( popped, newStact ) ->
                { stact = newStact
                , layout =
                    (Maybe.map
                        (\start -> Dict.update seg (update <| (::) <| Arc side start { x = x, dir = V }))
                        popped
                        |> Maybe.withDefault identity
                    )
                        layout
                }
           )


update : (List a -> List a) -> Maybe (List a) -> Maybe (List a)
update f =
    Maybe.withDefault [] >> f >> Just


east : Int -> State -> Maybe State
east seg state =
    --let
    --    matchTop st =
    --        Stact.peek st
    --            |> Maybe.map Tuple.first
    --            |> (==) (Just seg)
    --in
    --if matchTop state.above then
    --else if matchTop state.below then
    --else
    Stact.pop state.above
        |> Maybe.andThen
            (\( ( s, start ), above ) ->
                if s == seg then
                    { state
                        | layout =
                            Dict.update seg
                                (update <| (::) <| Arc N start { x = state.x - 1, dir = E })
                                state.layout
                        , x = state.x
                        , above = above
                    }
                        |> Just

                else
                    Nothing
            )
        |> Maybe.withDefault
            (Stact.pop state.below
                |> Maybe.andThen
                    (\( ( s, start ), below ) ->
                        if s == seg then
                            { state
                                | layout =
                                    Dict.update seg
                                        (update <|
                                            (::) (Arc S start { x = state.x - 1, dir = E })
                                        )
                                        state.layout
                                , x = state.x
                                , below = below
                            }
                                |> Just

                        else
                            Nothing
                    )
                |> Maybe.withDefault
                    { state
                        | above =
                            Stact.push seg { x = state.x - 1, dir = E } state.above
                        , x = state.x
                    }
            )
        |> Just


next : Orient.Terminal -> Maybe State -> Maybe State
next terminal =
    Maybe.andThen (west terminal.w)
        >> Maybe.map (north terminal.n)
        >> Maybe.map (south terminal.s)
        >> Maybe.map incX
        >> Maybe.andThen (east terminal.e)


end : State -> State
end state =
    case ( Stact.pop state.above, Stact.pop state.below ) of
        ( Just ( ( s1, x1 ), above ), Just ( ( s2, x2 ), below ) ) ->
            if s1 == s2 then
                { state
                    | layout =
                        Dict.update s1
                            (Maybe.withDefault []
                                >> (::) (Arc N x1 { x = state.x, dir = V })
                                >> (::) (Arc S x2 { x = state.x, dir = V })
                                >> Just
                            )
                            state.layout
                    , above = above
                    , below = below
                    , x = state.x + 1
                }
                    |> end

            else
                state

        _ ->
            state


build : List Orient.Terminal -> Maybe Success
build terminals =
    case terminals of
        [] ->
            -- handle unknot specially, since by our encoding it has "no" arcs
            Nothing

        --Just
        --    { layout = Dict.singleton 0 [ Arc S {} 1, Arc N 0 1 ]
        --    , width = 1
        --    }
        first :: _ ->
            -- TODO handle first better
            List.foldl next (Just <| init first) terminals
                |> Maybe.map (end >> Debug.log "after end")
                |> Maybe.map
                    (\st ->
                        { layout = Dict.map (always List.reverse) st.layout
                        , width = st.x
                        }
                    )
