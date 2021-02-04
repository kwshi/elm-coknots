module GaussCode.Common exposing
    ( Crossing
    , GaussCode
    , Order(..)
    , Sign(..)
    , crossingToString
    , gaussCodeToString
    )


type alias GaussCode =
    List Crossing


type alias Crossing =
    { label : Int, order : Order, sign : Sign }


type Sign
    = Plus
    | Minus


type Order
    = Over
    | Under


crossingToString : Crossing -> String
crossingToString { label, order, sign } =
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


gaussCodeToString : GaussCode -> String
gaussCodeToString =
    List.map crossingToString >> String.join " "
