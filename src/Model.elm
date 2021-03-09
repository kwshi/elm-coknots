module Model exposing (..)

import Browser.Navigation as Nav


type alias Model =
    { nav : Nav.Key
    , input :
        { cursor : Maybe Int
        , content : String
        }
    , hoverErr : Maybe Int
    }


type Msg
    = Nop
    | Input ( Maybe Int, String )
    | Selection (Maybe ( Int, Int ))
    | SetInput String
    | HoverIn Int
    | HoverOut
    | RotLeft
    | RotRight
