module GaussCode exposing
    ( Crossing
    , GaussCode
    , Order
    , Sign
    , crossingParser
    , crossingToString
    , parser
    , toString
    )

import GaussCode.Common as Common
import GaussCode.Parser as Parser


type alias GaussCode =
    Common.GaussCode


type alias Crossing =
    Common.Crossing


type alias Sign =
    Common.Sign


type alias Order =
    Common.Order


parser : Parser.Parser c GaussCode
parser =
    Parser.gaussCode


toString : GaussCode -> String
toString =
    Common.gaussCodeToString


crossingParser : Parser.Parser c Crossing
crossingParser =
    Parser.crossing


crossingToString : Crossing -> String
crossingToString =
    Common.crossingToString
