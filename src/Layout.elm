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
        { state
            | above = Stact.push seg state.x state.above
            , below = Stact.push seg state.x state.below
            , x = state.x + 1
        }
            |> Just

    else
        { state
            | layout =
                Dict.insert seg
                    [ Spine (state.x - 1) state.x ]
                    state.layout
            , x = state.x + 1
        }
            |> Just



--Stact.shiftPopEither seg ( state.above, state.below )
--    |> Maybe.map
--        (\( start, which, ( newAbove, newBelow ) ) ->
--            { state
--                | above = newAbove
--                , below = newBelow
--                , layout =
--                    Dict.update
--                        seg
--                        (Maybe.withDefault []
--                            >> (::)
--                                (Arc
--                                    (case which of
--                                        Stact.First ->
--                                            N
--                                        Stact.Second ->
--                                            S
--                                    )
--                                    start.x
--                                    state.x
--                                )
--                            >> Just
--                        )
--                        state.layout
--            }
--        )
--    |> Maybe.withDefault
--        (if seg == state.first.w then
--            -- special handling for first
--            { state
--                | above = Stact.push seg state.above
--                , below = Stact.push seg state.below
--            }
--         else
--            -- if it's not in the above or below,
--            -- then it must be along the spine
--            -- from the previous terminal
--            { state
--                | layout =
--                    Dict.insert
--                        seg
--                        [ Spine (state.x - 1) state.x ]
--                        state.layout
--                , x = state.x + 1
--            }
--        )


next : Calc.Terminal -> Maybe State -> Maybe State
next terminal =
    Maybe.andThen (\state -> west terminal.w state)


build : List Calc.Terminal -> Maybe Layout
build terminals =
    case terminals of
        [] ->
            -- handle unknot specially, since by our encoding it has "no" arcs
            Just <| Dict.singleton 0 [ Arc S 0 1, Arc N 1 0 ]

        first :: _ ->
            -- TODO handle first better
            List.foldl next (Just <| init first) terminals
                |> Maybe.map .layout
