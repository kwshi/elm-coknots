module Stact exposing (..)

import Dict


type Stact comparable a
    = Stact
        { stack : List ( comparable, a )
        , dict : Dict.Dict comparable a
        }


type Which
    = First
    | Second


empty : Stact comparable a
empty =
    Stact { stack = [], dict = Dict.empty }


singleton : comparable -> a -> Stact comparable a
singleton k v =
    Stact { stack = [ ( k, v ) ], dict = Dict.singleton k v }


push : comparable -> a -> Stact comparable a -> Stact comparable a
push k v (Stact { stack, dict }) =
    Stact
        { stack = ( k, v ) :: stack
        , dict = Dict.insert k v dict
        }


peek : Stact comparable a -> Maybe ( comparable, a )
peek (Stact { stack }) =
    List.head stack


pop : Stact comparable a -> Maybe ( ( comparable, a ), Stact comparable a )
pop (Stact { stack, dict }) =
    case stack of
        [] ->
            Nothing

        ( k, v ) :: newStack ->
            Just
                ( ( k, v )
                , Stact
                    { stack = newStack
                    , dict = Dict.remove k dict
                    }
                )


popBoth :
    ( Stact comparable a, Stact comparable a )
    ->
        Maybe
            ( ( ( comparable, a ), ( comparable, a ) )
            , ( Stact comparable a, Stact comparable a )
            )
popBoth ( a, b ) =
    case ( pop a, pop b ) of
        ( Just ( ( k1, v1 ), s1 ), Just ( ( k2, v2 ), s2 ) ) ->
            Just ( ( ( k1, v1 ), ( k2, v2 ) ), ( s1, s2 ) )

        _ ->
            Nothing


popBothEqual :
    ( Stact comparable a, Stact comparable a )
    ->
        Maybe
            ( ( comparable, ( a, a ) )
            , ( Stact comparable a, Stact comparable a )
            )
popBothEqual =
    Maybe.andThen
        (\( ( ( k1, v1 ), ( k2, v2 ) ), ( s1, s2 ) ) ->
            if k1 == k2 then
                Just ( ( k1, ( v1, v2 ) ), ( s1, s2 ) )

            else
                Nothing
        )
        << popBoth


pushOrPop : comparable -> a -> Stact comparable a -> ( Maybe a, Stact comparable a )
pushOrPop k v st =
    case pop st of
        Nothing ->
            ( Nothing, push k v st )

        Just ( ( kTop, vTop ), newSt ) ->
            if kTop /= k then
                ( Nothing, push k v st )

            else
                ( Just vTop, newSt )


member : comparable -> Stact comparable a -> Bool
member k (Stact { dict }) =
    Dict.member k dict


popUntil :
    comparable
    -> (comparable -> a -> acc -> acc)
    -> acc
    -> Stact comparable a
    -> Maybe ( a, acc, Stact comparable a )
popUntil target f =
    let
        go acc st =
            Maybe.andThen
                (\( ( k, v ), newSt ) ->
                    if k == target then
                        Just ( v, acc, newSt )

                    else
                        go (f k v acc) newSt
                )
                (pop st)
    in
    go


shiftPop :
    comparable
    -> { from : Stact comparable a, to : Stact comparable a }
    -> Maybe ( a, { from : Stact comparable a, to : Stact comparable a } )
shiftPop pivot =
    let
        go { from, to } =
            Maybe.andThen
                (\( ( k, v ), newFrom ) ->
                    if k == pivot then
                        Just ( v, { from = newFrom, to = to } )

                    else
                        go { from = newFrom, to = push k v to }
                )
                (pop from)
    in
    go


shiftPopEither :
    comparable
    -> ( Stact comparable a, Stact comparable a )
    -> Maybe ( a, Which, ( Stact comparable a, Stact comparable a ) )
shiftPopEither pivot ( s1, s2 ) =
    if member pivot s1 then
        shiftPop pivot { from = s1, to = s2 }
            |> Maybe.map (\( v, { from, to } ) -> ( v, First, ( from, to ) ))

    else if member pivot s2 then
        shiftPop pivot { from = s2, to = s1 }
            |> Maybe.map (\( v, { from, to } ) -> ( v, Second, ( to, from ) ))

    else
        Nothing
