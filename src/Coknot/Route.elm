module Coknot.Route exposing (..)

import Coknot.Layout as Layout
import Coknot.Orient as Orient
import Lens
import Stact


type alias State =
    { layout : Layout.Layout
    , first : Orient.Terminal
    , above : Stact.Stact Seg Layout.Endpoint
    , below : Stact.Stact Seg Layout.Endpoint
    , x : Int
    }


stateLens :
    { above : Lens.Prop State (Stact.Stact Seg Layout.Endpoint)
    , below : Lens.Prop State (Stact.Stact Seg Layout.Endpoint)
    , x : Lens.Prop State Int
    , layout : Lens.Prop State Layout.Layout
    }
stateLens =
    { above = Lens.prop .above <| \a st -> { st | above = a }
    , below = Lens.prop .below <| \a st -> { st | below = a }
    , x = Lens.prop .x <| \a st -> { st | x = a }
    , layout = Lens.prop .layout <| \a st -> { st | layout = a }
    }


incX : { a | x : Int } -> { a | x : Int }
incX a =
    { a | x = a.x + 1 }


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


addArcs : Int -> List Layout.Arc -> State -> State
addArcs seg =
    stateLens.layout.edit << Layout.addArcs seg


addArc : Int -> Layout.Arc -> State -> State
addArc seg =
    stateLens.layout.edit << Layout.addArc seg


emit :
    Lens.Prop State (Stact.Stact Int Layout.Endpoint)
    -> { seg : Int, side : Layout.Side, end : Layout.Endpoint }
    -> State
    -> State
emit stact { seg, side, end } state =
    let
        maybeAddArc =
            Maybe.map
                (\start ->
                    addArc seg
                        { start = start
                        , end = end
                        , side = side
                        }
                )
                >> Maybe.withDefault identity
    in
    stact.get state
        |> Stact.pushOrPop seg end
        |> (\( popped, poppedStact ) ->
                state
                    |> stact.set poppedStact
                    |> maybeAddArc popped
           )


shift :
    { seg : Int
    , dir : Layout.Dir
    , fromStact : Lens.Prop State (Stact.Stact Int Layout.Endpoint)
    , toStact : Lens.Prop State (Stact.Stact Int Layout.Endpoint)
    , fromSide : Layout.Side
    , toSide : Layout.Side
    }
    -> State
    -> Maybe State
shift { seg, dir, fromStact, toStact, fromSide, toSide } =
    let
        go state =
            Stact.pop (fromStact.get state)
                |> Maybe.andThen (handle state)

        handle state ( ( fromSeg, fromStart ), fromPopped ) =
            let
                addArcFacing d =
                    addArc seg
                        { side = fromSide
                        , start = fromStart
                        , end = { x = state.x, dir = d }
                        }
            in
            state
                |> fromStact.set fromPopped
                |> incX
                |> (if fromSeg == seg then
                        addArcFacing dir >> Just

                    else
                        addArcFacing Layout.V
                            >> emit toStact
                                { seg = fromSeg
                                , side = toSide
                                , end = { x = state.x, dir = Layout.V }
                                }
                            >> go
                   )
    in
    go


handleWest : Int -> State -> Maybe State
handleWest seg state =
    (if Stact.member seg state.above then
        shift
            { seg = seg
            , dir = Layout.W
            , fromStact = stateLens.above
            , toStact = stateLens.below
            , fromSide = Layout.N
            , toSide = Layout.S
            }

     else if Stact.member seg state.below then
        shift
            { seg = seg
            , dir = Layout.W
            , fromStact = stateLens.below
            , toStact = stateLens.above
            , fromSide = Layout.S
            , toSide = Layout.N
            }

     else if state.first.w == seg then
        -- TODO handle first better
        stateLens.above.edit
            (Stact.push seg { x = state.x, dir = Layout.W })
            >> stateLens.below.edit
                (Stact.push seg { x = state.x, dir = Layout.W })
            >> incX
            >> Just

     else
        Just
    )
        state


vert :
    Lens.Prop State (Stact.Stact Int Layout.Endpoint)
    -> Layout.Side
    -> Int
    -> State
    -> State
vert stact side seg state =
    emit stact
        { seg = seg
        , side = side
        , end = { x = state.x, dir = Layout.V }
        }
        state


handleNorth : Int -> State -> State
handleNorth =
    vert stateLens.above Layout.N


handleSouth : Int -> State -> State
handleSouth =
    vert stateLens.below Layout.S


update : (List a -> List a) -> Maybe (List a) -> Maybe (List a)
update f =
    Maybe.withDefault [] >> f >> Just


east : Int -> State -> Maybe State
east seg state =
    Stact.pop state.above
        |> Maybe.andThen
            (\( ( s, start ), above ) ->
                if s == seg then
                    state
                        |> stateLens.layout.edit
                            (Layout.addArc seg
                                { side = Layout.N
                                , start = start
                                , end = { x = state.x, dir = Layout.E }
                                }
                            )
                        |> stateLens.above.set above
                        |> incX
                        |> Just

                else
                    Nothing
            )
        |> Maybe.withDefault
            (Stact.pop state.below
                |> Maybe.andThen
                    (\( ( s, start ), below ) ->
                        if s == seg then
                            state
                                |> stateLens.layout.edit
                                    (Layout.addArc seg
                                        { side = Layout.S
                                        , start = start
                                        , end = { x = state.x, dir = Layout.E }
                                        }
                                    )
                                |> stateLens.below.set below
                                |> incX
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
    stateLens.layout.edit (Layout.addCrossing st.x term) st


next : Orient.Terminal -> Maybe State -> Maybe State
next terminal =
    Maybe.andThen (handleWest terminal.w)
        >> Maybe.map
            (crossing terminal
                >> handleNorth terminal.n
                >> handleSouth terminal.s
                >> incX
            )
        >> Maybe.andThen (east terminal.e)


finalize : State -> State
finalize state =
    let
        arc side start =
            { side = side
            , start = start
            , end = { x = state.x, dir = Layout.V }
            }
    in
    (Stact.popBothEqual ( state.above, state.below )
        |> Maybe.map
            (\( ( seg, ( aboveStart, belowStart ) ), ( above, below ) ) ->
                addArcs seg
                    [ arc Layout.N aboveStart
                    , arc Layout.S belowStart
                    ]
                    >> stateLens.above.set above
                    >> stateLens.below.set below
                    >> incX
                    >> finalize
            )
        |> Maybe.withDefault identity
    )
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
                |> Maybe.map (finalize >> Debug.log "after end")
                |> Maybe.map
                    (\st ->
                        { layout = Layout.finalize st.layout
                        , width = st.x
                        }
                    )
