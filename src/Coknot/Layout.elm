module Coknot.Layout exposing (..)

import Coknot.Orient as Orient
import Dict
import Lens
import Stact


type Side
    = N
    | S


type Dir
    = V
    | W
    | E


type alias Endpoint =
    { dir : Dir
    , x : Int
    }


type alias Arc =
    { side : Side, start : Endpoint, end : Endpoint }


type alias Stroke =
    List Arc


type alias Layout =
    { strokes : Dict.Dict Int Stroke
    , crossings : Dict.Dict Int Orient.Terminal
    , largest : Int
    }


lens :
    { strokes : Lens.Prop Layout (Dict.Dict Int Stroke)
    , crossings : Lens.Prop Layout (Dict.Dict Int Orient.Terminal)
    , largest : Lens.Prop Layout Int
    }
lens =
    { strokes = Lens.prop .strokes <| \a r -> { r | strokes = a }
    , crossings = Lens.prop .crossings <| \a r -> { r | crossings = a }
    , largest = Lens.prop .largest <| \a r -> { r | largest = a }
    }


init : Layout
init =
    { strokes = Dict.empty
    , crossings = Dict.empty
    , largest = 0
    }


maximum : (a -> Int) -> Int -> List a -> Int
maximum f =
    List.foldl <| max << f


arcSize : Arc -> Int
arcSize { end, start } =
    end.x - start.x


addArcs : Int -> List Arc -> Layout -> Layout
addArcs seg arcs =
    lens.strokes.edit
        (Dict.update seg (Maybe.withDefault [] >> (++) arcs >> Just))
        >> lens.largest.edit (\l -> maximum arcSize l arcs)


addArc : Int -> Arc -> Layout -> Layout
addArc seg =
    List.singleton >> addArcs seg


finalize : Layout -> Layout
finalize =
    lens.strokes.edit <| Dict.map <| always List.reverse


addCrossing : Int -> Orient.Terminal -> Layout -> Layout
addCrossing x t =
    lens.crossings.edit <| Dict.insert x t
