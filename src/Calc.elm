module Calc exposing (..)

import Dict
import GaussCode as Gc
import GaussCode.Common as Gcc

type alias Terminal =
    { n : Int
    , e : Int
    , s : Int
    , w : Int
    }


zipNeighbors : List a -> List ( a, a, a )
zipNeighbors l =
    case l of
        [] ->
            []

        [ _ ] ->
            []

        a :: b :: rest ->
            let
                go acc x y rem =
                    case rem of
                        [] ->
                            ( y, a, b ) :: List.reverse (( x, y, a ) :: acc)

                        z :: newRem ->
                            go (( x, y, z ) :: acc) y z newRem
            in
            go [] a b rest


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
        Once _ ->
            Nothing

        Twice t ->
            Just t


getTerminals : Gc.GaussCode -> Maybe (List Terminal)
getTerminals waypoints =
    zipNeighbors waypoints
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
    = Once ( Int, Int )
    | Twice Terminal


markTerminal :
    ( Gc.Crossing, Gc.Crossing, Gc.Crossing )
    -> Maybe (Dict.Dict Int Visited)
    -> Maybe (Dict.Dict Int Visited)
markTerminal ( prev, curr, next ) =
    Maybe.andThen
        (\terminals ->
            case Dict.get curr.label terminals of
                -- first visit, left to right
                Nothing ->
                    Dict.insert curr.label
                        (Once ( prev.label, next.label ))
                        terminals
                        |> Just

                Just (Once ( w, e )) ->
                    -- second visit, vertical
                    Dict.insert curr.label
                        (case (curr.sign, curr.order) of
                            (Gcc.Plus, Gcc.Under) ->
                                Twice { n = next.label, e = e, s = prev.label, w = w }

                            (Gcc.Minus, Gcc.Under) ->
                                Twice { n = prev.label, e = e, s = next.label, w = w }

                            (Gcc.Minus, Gcc.Over) ->
                                Twice { n = next.label, e = e, s = prev.label, w = w }

                            (Gcc.Plus, Gcc.Over) ->
                                Twice { n = prev.label, e = e, s = next.label, w = w }
                        )
                        terminals
                        |> Just

                Just (Twice _) ->
                    Nothing
        )


