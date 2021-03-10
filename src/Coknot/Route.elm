module Coknot.Route exposing (..)

import Coknot.Layout as Layout
import Coknot.Orient as Orient
import Dict
import Set
import Stact


type alias State =
    { layout : Layout.Layout
    , first : Orient.Terminal
    , above : Stact.Stact Seg Layout.Endpoint
    , below : Stact.Stact Seg Layout.Endpoint
    , x : Int
    }


type alias Seg =
    Int


type alias Success =
    { layout : Layout.Layout
    , width : Int
    }


init : Orient.Terminal -> State
init first =
    { x = 0
    , first = first
    , above = Stact.empty
    , below = Stact.empty
    , layout = Layout.init
    }


type alias ShiftState =
    { x : Int
    , from : Stact.Stact Seg Layout.Endpoint
    , to : Stact.Stact Seg Layout.Endpoint
    , layout : Layout.Layout
    }


shift : ( Int, Layout.Side, Layout.Side ) -> ShiftState -> Maybe ShiftState
shift ( seg, fromSide, toSide ) state =
    Stact.popUntil seg
        (\s start { x, layout, to } ->
            to
                |> Stact.pushOrPop s { x = x, dir = Layout.V }
                |> (\( popped, newTo ) ->
                        { x = x + 1
                        , to = newTo
                        , layout =
                            Layout.addArc s
                                { side = fromSide
                                , start = start
                                , end = { x = x, dir = Layout.V }
                                }
                                layout
                                |> (case popped of
                                        Nothing ->
                                            identity

                                        Just otherStart ->
                                            Layout.addArc s
                                                { side = toSide
                                                , start = otherStart
                                                , end = { x = x, dir = Layout.V }
                                                }
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
                        Layout.addArc seg
                            { side = fromSide, start = start, end = { x = x, dir = Layout.W } }
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
            |> shift ( seg, Layout.N, Layout.S )
            |> Maybe.map
                (\{ x, from, to, layout } ->
                    { state
                        | x = x + 1
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
            |> shift ( seg, Layout.S, Layout.N )
            |> Maybe.map
                (\{ x, from, to, layout } ->
                    { state
                        | x = x + 1
                        , below = from
                        , above = to
                        , layout = layout
                    }
                )

    else if state.first.w == seg then
        -- TODO handle first better
        { state
            | above =
                Stact.push seg
                    { x = state.x, dir = Layout.W }
                    state.above
            , below =
                Stact.push
                    seg
                    { x = state.x, dir = Layout.W }
                    state.below
            , layout = state.layout
            , x = state.x + 1
        }
            |> Just

    else
        Just state


north : Int -> State -> State
north seg state =
    { seg = seg
    , stact = state.above
    , side = Layout.N
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
    , side = Layout.S
    , x = state.x
    , layout = state.layout
    }
        |> vert
        |> (\{ stact, layout } ->
                { state | below = stact, layout = layout }
           )


vert :
    { seg : Int
    , stact : Stact.Stact Seg Layout.Endpoint
    , side : Layout.Side
    , x : Int
    , layout : Layout.Layout
    }
    ->
        { stact : Stact.Stact Seg Layout.Endpoint
        , layout : Layout.Layout
        }
vert { seg, stact, side, x, layout } =
    Stact.pushOrPop seg { x = x, dir = Layout.V } stact
        |> (\( popped, newStact ) ->
                { stact = newStact
                , layout =
                    (Maybe.map
                        (\start -> Layout.addArc seg { side = side, start = start, end = { x = x, dir = Layout.V } })
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
    Stact.pop state.above
        |> Maybe.andThen
            (\( ( s, start ), above ) ->
                if s == seg then
                    { state
                        | layout =
                            Layout.addArc seg
                                { side = Layout.N
                                , start = start
                                , end = { x = state.x, dir = Layout.E }
                                }
                                state.layout
                        , x = state.x + 1
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
                                    Layout.addArc seg
                                        { side = Layout.S
                                        , start = start
                                        , end = { x = state.x, dir = Layout.E }
                                        }
                                        state.layout
                                , x = state.x + 1
                                , below = below
                            }
                                |> Just

                        else
                            Nothing
                    )
                |> Maybe.withDefault
                    { state
                        | above =
                            Stact.push seg
                                { x = state.x, dir = Layout.E }
                                state.above
                        , x = state.x + 1
                    }
            )
        |> Just


crossing : Orient.Terminal -> State -> State
crossing term st =
    let
        { layout } =
            st
    in
    { st
        | layout =
            { layout
                | crossings =
                    Dict.insert
                        st.x
                        term
                        layout.crossings
            }
    }


next : Orient.Terminal -> Maybe State -> Maybe State
next terminal =
    Maybe.andThen (west terminal.w)
        >> Maybe.map (crossing terminal)
        >> Maybe.map (north terminal.n >> south terminal.s)
        >> Maybe.map incX
        >> Maybe.andThen (east terminal.e)


end : State -> State
end state =
    case ( Stact.pop state.above, Stact.pop state.below ) of
        ( Just ( ( s1, x1 ), above ), Just ( ( s2, x2 ), below ) ) ->
            if s1 == s2 then
                { state
                    | layout =
                        Layout.addArcs
                            s1
                            [ { side = Layout.N
                              , start = x1
                              , end = { x = state.x, dir = Layout.V }
                              }
                            , { side = Layout.S
                              , start = x2
                              , end = { x = state.x, dir = Layout.V }
                              }
                            ]
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
                        { layout = Layout.finalize st.layout
                        , width = st.x
                        }
                    )
