module Gc exposing
    ( Waypoint
    , Gc
    , Order(..)
    , Sign(..)
    , waypointToString
    , toString
    )

type alias Gc =
    List Waypoint


type alias Waypoint =
    { label : Int, order : Order, sign : Sign }


type Sign
    = Plus
    | Minus


type Order
    = Over
    | Under


waypointToString : Waypoint -> String
waypointToString { label, order, sign } =
    String.fromInt label
        ++ (case order of
                Over ->
                    "o"

                Under ->
                    "u"
           )
        ++ (case sign of
                Plus ->
                    "+"

                Minus ->
                    "-"
           )


toString : Gc -> String
toString =
    List.map waypointToString >> String.join " "
