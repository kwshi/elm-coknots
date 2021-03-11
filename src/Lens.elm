module Lens exposing (..)


type alias Prop r a =
    { get : r -> a
    , set : a -> r -> r
    , edit : (a -> a) -> r -> r
    }


prop : (r -> a) -> (a -> r -> r) -> Prop r a
prop get set =
    { get = get
    , set = set
    , edit = \f r -> (get r |> f |> set) r
    }
