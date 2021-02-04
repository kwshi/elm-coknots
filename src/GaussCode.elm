module GaussCode exposing
    ( Crossing
    , GaussCode
    , parser
    , toString
    , crossingParser
    , crossingToString
    )

import GaussCode.Common as Common
import GaussCode.Parser as Parser


type alias GaussCode =
    Common.GaussCode


type alias Crossing =
    Common.Crossing


parser : Parser.Parser c GaussCode
parser = Parser.gaussCode

toString : GaussCode -> String
toString = Common.gaussCodeToString

crossingParser : Parser.Parser c Crossing
crossingParser = Parser.crossing

crossingToString : Crossing -> String
crossingToString = Common.crossingToString
