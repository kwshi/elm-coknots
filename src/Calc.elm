module Calc exposing (..)

import Dict
import Gc


type Axis = H | V

type alias Terminal =
    { n : Int
    , e : Int
    , s : Int
    , w : Int
    , dominant : Axis
    }


type alias Neighbors a =
    { prev : a
    , prevSeg : Int
    , curr : a
    , nextSeg : Int
    , next : a
    }


zipNeighbors : List a -> List (Neighbors a)
zipNeighbors l =
    case l of
        [] ->
            []

        [ _ ] ->
            []

        a :: b :: rest ->
            let
                go acc i x y rem =
                    case rem of
                        [] ->
                            Neighbors y 0 a 1 b
                                :: List.reverse (Neighbors x i y 0 a :: acc)

                        z :: newRem ->
                            go (Neighbors x i y (i + 1) z :: acc) (i + 1) y z newRem
            in
            go [] 1 a b rest


liftMaybe : List (Maybe a) -> Maybe (List a)
liftMaybe =
    let
        go acc l =
            case l of
                [] ->
                    Just (List.reverse acc)

                (Just a) :: rest ->
                    go (a :: acc) rest

                Nothing :: _ ->
                    Nothing
    in
    go []


getTerminal : Visited -> Maybe Terminal
getTerminal v =
    case v of
        Once _ _ _ ->
            Nothing

        Twice t ->
            Just t


getTerminals : Gc.Gc -> Maybe (List Terminal)
getTerminals waypoints =
    zipNeighbors waypoints
        |> Debug.log "zipneighbors"
        |> List.foldl markTerminal (Just Dict.empty)
        |> Maybe.andThen
            (\terminals ->
                waypoints
                    |> List.map
                        (\wp ->
                            Dict.get wp.label terminals
                                |> Maybe.andThen getTerminal
                        )
                    |> liftMaybe
            )


type Visited
    = Once ( Int, Int ) Gc.Order Gc.Sign
    | Twice Terminal


markTerminal :
    Neighbors Gc.Waypoint
    -> Maybe (Dict.Dict Int Visited)
    -> Maybe (Dict.Dict Int Visited)
markTerminal { prev, prevSeg, curr, nextSeg, next } =
    Maybe.andThen
        (\terminals ->
            case Dict.get curr.label terminals of
                -- first visit, left to right
                Nothing ->
                    Dict.insert curr.label
                        (Once ( prevSeg, nextSeg ) curr.order curr.sign)
                        terminals
                        |> Just

                Just (Once ( w, e ) order sign) ->
                    let
                        dominant = 
                          case curr.order of
                            Gc.Over -> V
                            Gc.Under -> H

                        make n s = {n = n, e = e, s = s, w = w, dominant = dominant}
                        up = make nextSeg prevSeg
                        down = make prevSeg nextSeg
                    in
                    -- second visit, vertical
                    if curr.sign /= sign then Nothing else
                    if curr.order == order then Nothing else
                    Dict.insert curr.label
                        (case ( curr.sign, curr.order ) of
                            ( Gc.Plus, Gc.Under ) ->
                                Twice up

                            ( Gc.Minus, Gc.Under ) ->
                                Twice down

                            ( Gc.Minus, Gc.Over ) ->
                                Twice up

                            ( Gc.Plus, Gc.Over ) ->
                                Twice down
                        )
                        terminals
                        |> Just

                Just (Twice _) ->
                    Nothing
        )
