module Layout exposing (..)

import Calc
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
    , first : Calc.Terminal
    , above : Stact.Stact Int Int
    , below : Stact.Stact Int Int
    , x : Int
    }


init : Calc.Terminal -> State
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


shift : Int -> ShiftState -> Maybe ShiftState
shift seg state =
    Stact.popUntil seg
        (\s start { x, layout, to } ->
            { x = x + 1
            , to = Stact.push s start to
            , layout = Dict.update s (initPush <| Arc N start x) layout
            }
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
                    , layout = Dict.update seg (initPush <| Arc N start x) layout
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
            |> shift seg
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
            |> shift seg
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
                    -- better not be the last strand
                    state
            )
        |> Just


next : Calc.Terminal -> Maybe State -> Maybe State
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


build : List Calc.Terminal -> Maybe Layout
build terminals =
    case terminals of
        [] ->
            -- handle unknot specially, since by our encoding it has "no" arcs
            Just <| Dict.singleton 0 [ Arc S 0 1, Arc N 1 0 ]

        first :: _ ->
            -- TODO handle first better
            List.foldl next (Just <| init first) terminals
                |> Maybe.map (.layout >> Dict.map (always List.reverse))
