module Coknot.Postprocess exposing (..)

import Coknot.Layout as Layout
import Coknot.Orient as Orient
import Dict
import List.Extra
import Set


type alias Options =
    { skipTrivial : Bool
    }


type alias Diagram =
    { arcs : List { id : Int, stroke : Layout.Stroke }
    , largest : Int
    , crossings : List Crossing
    }


type alias Crossing =
    { x : Int
    , terminal : Orient.Terminal
    }


isTrivial : Layout.Arc -> Bool
isTrivial { start, end } =
    case ( end.x - start.x, start.dir, end.dir ) of
        ( 1, Layout.E, Layout.W ) ->
            True

        _ ->
            False


process : Options -> Layout.Layout -> Diagram
process opts layout =
    let
        trivXs =
            Dict.values layout.strokes
                |> List.concat
                |> trivialXs

        ( state, arcs ) =
            Dict.toList (Debug.log "layout.strokes" layout.strokes)
                |> List.Extra.mapAccuml
                    (\{ skip } ( id, stroke ) ->
                        List.Extra.mapAccuml
                            (\sk ({ start, end } as arc) ->
                                sk start.x
                                    |> (\( newStartX, SkipF sk1 ) ->
                                            sk1 end.x
                                                |> (\( newEndX, SkipF sk2 ) ->
                                                        ( sk2
                                                        , { arc
                                                            | start = { start | x = newStartX }
                                                            , end = { end | x = newEndX }
                                                          }
                                                        )
                                                   )
                                       )
                            )
                            skip
                            stroke
                            |> (\( skf, newStroke ) -> ( { skip = skf }, { id = id, stroke = newStroke } ))
                    )
                    { skip = skipX trivXs
                    }

        crossings =
            List.map
                (\( x, terminal ) ->
                    { x = state.skip x |> Tuple.first, terminal = terminal }
                )
                (Dict.toList layout.crossings)
    in
    { arcs = Debug.log "arcs" arcs
    , crossings = Debug.log "new crossings" crossings
    , largest = 0
    }


trivialXs : List Layout.Arc -> Set.Set Int
trivialXs =
    List.foldl
        (\({ start, end } as arc) ->
            if isTrivial arc then
                Set.insert start.x >> Set.insert end.x

            else
                identity
        )
        Set.empty


type SkipF
    = SkipF (Int -> ( Int, SkipF ))


skipX : Set.Set Int -> Int -> ( Int, SkipF )
skipX xs =
    let
        go ({ curr, new, mapping } as acc) x =
            if x < curr then
                ( Dict.get x mapping
                    |> Maybe.withDefault 0
                , SkipF (go acc)
                )

            else
                go
                    { curr = curr + 1
                    , new =
                        if Set.member curr xs then
                            new

                        else
                            new + 1
                    , mapping = Dict.insert curr new mapping
                    }
                    x
    in
    go
        { curr = 0
        , new = 0
        , mapping = Dict.empty
        }
