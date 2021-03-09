module Coknot.Layout exposing (..)

import Dict
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
    , largest : Int
    }


init : Layout
init =
    { strokes = Dict.empty
    , largest = 0
    }


addArcs : Int -> List Arc -> Layout -> Layout
addArcs seg arcs layout =
    { strokes =
        Dict.update
            seg
            (Maybe.withDefault [] >> (++) arcs >> Just)
            layout.strokes
    , largest =
        List.foldl (\arc m -> max m (arc.end.x - arc.start.x)) layout.largest arcs
    }


addArc : Int -> Arc -> Layout -> Layout
addArc seg =
    List.singleton >> addArcs seg


finalize : Layout -> Layout
finalize layout =
    { layout
        | strokes = Dict.map (always List.reverse) layout.strokes
    }
