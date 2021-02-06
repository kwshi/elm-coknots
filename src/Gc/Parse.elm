module Gc.Parse exposing (..)

import Gc


type BadWaypoint
    = ExpectingLabel
    | ExpectingOrder Char
    | ExpectingSign Char
    | MissingSign
    | MissingOrder
    | ExpectingEnd


type alias Error =
    { pos : Int
    , index : Int
    , char : Int
    , err : BadWaypoint
    }


waypoint :
    List Char
    ->
        Result
            { char : Int, err : BadWaypoint }
            { label : Int, order : Gc.Order, sign : Gc.Sign }
waypoint =
    let
        label i acc cs =
            case cs of
                [] ->
                    Err { char = i, err = MissingOrder }

                c :: rest ->
                    if Char.isDigit c then
                        label (i + 1) (c :: acc) rest

                    else
                        List.reverse acc
                            |> String.fromList
                            |> String.toInt
                            |> Result.fromMaybe { char = 0, err = ExpectingLabel }
                            |> Result.map (\n -> ( n, i, cs ))

        order i cs =
            case cs of
                [] ->
                    Err { char = i, err = MissingOrder }

                'o' :: rest ->
                    Ok ( Gc.Over, i + 1, rest )

                'u' :: rest ->
                    Ok ( Gc.Under, i + 1, rest )

                c :: _ ->
                    Err { char = i, err = ExpectingOrder c }

        sign i cs =
            case cs of
                [] ->
                    Err { char = i, err = MissingSign }

                [ '+' ] ->
                    Ok Gc.Plus

                [ '-' ] ->
                    Ok Gc.Minus

                [ c ] ->
                    Err { char = i, err = ExpectingSign c }

                _ ->
                    Err { char = i + 1, err = ExpectingEnd }
    in
    label 0 []
        >> Result.andThen
            (\( l, i1, cs1 ) ->
                order i1 cs1
                    |> Result.andThen
                        (\( o, i2, cs2 ) ->
                            sign i2 cs2
                                |> Result.map
                                    (\s -> { label = l, order = o, sign = s })
                        )
            )


stripSplit : (a -> Bool) -> List a -> List { pos : Int, run : List a }
stripSplit f =
    let
        pushIfNonempty =
            Maybe.map (\( i, run ) -> (::) { pos = i, run = List.reverse run })
                >> Maybe.withDefault identity
    in
    List.foldl
        (\a ( i, acc, st ) ->
            if f a then
                ( i + 1, pushIfNonempty st acc, Nothing )

            else
                ( i + 1
                , acc
                , Maybe.withDefault ( i, [] ) st
                    |> ((\( j, buf ) -> ( j, a :: buf )) >> Just)
                )
        )
        ( 0, [], Nothing )
        >> (\( _, runs, last ) -> pushIfNonempty last runs)
        >> List.reverse


validate : (Int -> a -> Result err ok) -> List a -> Result (List err) (List ok)
validate f =
    List.foldl
        (\a ( i, oks, errs ) ->
            case f i a of
                Ok o ->
                    ( i + 1, o :: oks, errs )

                Err e ->
                    ( i + 1, oks, e :: errs )
        )
        ( 0, [], [] )
        >> (\( _, oks, errs ) ->
                case errs of
                    [] ->
                        Ok <| List.reverse oks

                    _ ->
                        Err <| List.reverse errs
           )


gaussCode : String -> Result (List Error) Gc.Gc
gaussCode =
    String.toList
        >> stripSplit ((==) ' ')
        >> validate
            (\index { pos, run } ->
                waypoint run
                    |> Result.map
                        (\{ label, order, sign } ->
                            { label = label
                            , order = order
                            , sign = sign
                            , index = index
                            , pos = pos
                            }
                        )
                    |> Result.mapError
                        (\{ char, err } ->
                            { index = index
                            , pos = pos
                            , char = char
                            , err = err
                            }
                        )
            )
