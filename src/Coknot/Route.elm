module Coknot.Route exposing (..)

import Coknot.Orient as Orient
import Dict
import Set
import Stact


type Side
    = N
    | S


type Part
    = Arc Side Int Int
    | Spine Int Int


type alias Stroke =
    List Part


type alias Layout =
    Dict.Dict Int Stroke


type alias State =
    { layout : Layout
    , first : Orient.Terminal
    , above : Stact.Stact Int Int
    , below : Stact.Stact Int Int
    , x : Int
    }


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
    , from : Stact.Stact Int Int
    , to : Stact.Stact Int Int
    , layout : Layout
    }


shift : ( Int, Side, Side ) -> ShiftState -> Maybe ShiftState
shift ( seg, fromSide, toSide ) state =
    Stact.popUntil seg
        (\s start { x, layout, to } ->
            Stact.pushOrPop s x (Debug.log "TO BEFORE" to)
                |> (\( popped, newTo ) ->
                        { x = x + 1
                        , to = Debug.log "NEW TO" newTo
                        , layout =
                            Dict.update s (initPush <| Arc fromSide start x) layout
                                |> (case popped of
                                        Nothing ->
                                            identity

                                        Just otherStart ->
                                            Dict.update s (initPush <| Arc toSide otherStart x)
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
                    | x = x + 1
                    , to = to
                    , from = from
                    , layout =
                        Dict.update seg
                            (Maybe.withDefault []
                                >> (::) (Arc fromSide start x)
                                >> (::) (Spine x (x + 1))
                                >> Just
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
            | above = Stact.push seg state.x state.above
            , below = Stact.push seg state.x state.below
            , layout =
                Dict.update seg
                    (initPush <| Spine state.x (state.x + 1))
                    state.layout
            , x = state.x + 1
        }
            |> Just

    else
        { state
            | layout =
                Dict.insert seg
                    [ Spine (state.x - 1) state.x ]
                    state.layout
            , x = state.x
        }
            |> Just


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
    , stact : Stact.Stact Int Int
    , side : Side
    , x : Int
    , layout : Layout
    }
    ->
        { stact : Stact.Stact Int Int
        , layout : Layout
        }
vert { seg, stact, side, x, layout } =
    Stact.pushOrPop seg x stact
        |> (\( popped, newStact ) ->
                { stact = newStact
                , layout =
                    (Maybe.map
                        (\start -> Dict.update seg (initPush <| Arc side start x))
                        popped
                        |> Maybe.withDefault identity
                    )
                        layout
                }
           )


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
                                (Maybe.withDefault []
                                    >> (::) (Arc N start state.x)
                                    >> (::) (Spine (state.x - 1) state.x)
                                    >> Just
                                )
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
                                    Dict.update seg
                                        (Maybe.withDefault []
                                            >> (::) (Arc S start state.x)
                                            >> (::) (Spine (state.x - 1) state.x)
                                            >> Just
                                        )
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
                        | layout =
                            Dict.update seg
                                (initPush <| Spine (state.x - 1) state.x)
                                state.layout
                        , above =
                            Stact.push seg state.x state.above
                        , x = state.x + 1
                    }
            )
        |> Just


next : Orient.Terminal -> Maybe State -> Maybe State
next terminal =
    let
        _ =
            Debug.log "handling terminal" terminal
    in
    Debug.log "initial state"
        >> Maybe.andThen (west terminal.w)
        >> Maybe.map (Debug.log "after handling west")
        >> Maybe.map (north terminal.n)
        >> Maybe.map (Debug.log "after handling north")
        >> Maybe.map (south terminal.s)
        >> Maybe.map (Debug.log "after handling south")
        >> Maybe.map incX
        >> Maybe.map (Debug.log "incx")
        >> Maybe.andThen (east terminal.e)
        >> Maybe.map (Debug.log "after handling east")


end : State -> State
end state =
    case ( Stact.pop state.above, Stact.pop state.below ) of
        ( Just ( ( s1, x1 ), above ), Just ( ( s2, x2 ), below ) ) ->
            if s1 == s2 then
                { state
                    | layout =
                        Dict.update s1
                            (Maybe.withDefault []
                                >> (::) (Arc N x1 state.x)
                                >> (::) (Arc S x2 state.x)
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
            Just
                { layout = Dict.singleton 0 [ Arc S 0 1, Arc N 0 1 ]
                , width = 1
                }

        first :: _ ->
            -- TODO handle first better
            List.foldl next (Just <| init first) terminals
                |> Maybe.map end
                |> Debug.log "end"
                |> Maybe.map
                    (\st ->
                        { layout = Dict.map (always List.reverse) st.layout
                        , width = st.x
                        }
                    )
