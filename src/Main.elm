module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import GaussCode as Gc
import GaussCode.Parser as Gcp
import Html.Styled as Ht
import Html.Styled.Attributes as At
import Html.Styled.Events as Ev
import Parser.Advanced as Parser
import Url


type alias Model =
    { input : String
    , crossing : Maybe Gc.GaussCode
    }


type Msg
    = Nop
    | Input String


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> Nop
        , onUrlChange = \_ -> Nop
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url nav =
    ( { input = ""
      , crossing = Nothing
      }
    , Cmd.none
    )


view : Model -> Browser.Document Msg
view model =
    { title = "hi"
    , body = viewBody model |> List.map Ht.toUnstyled
    }


viewBody : Model -> List (Ht.Html Msg)
viewBody model =
  let _ = Debug.log "crossing" model in
    [ Ht.input [ At.value model.input, Ev.onInput Input ] []
    , Ht.div [] [ Ht.text model.input ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Input s ->
            ( { model
                | input = s
                , crossing = Parser.run Gcp.gaussCode s
                |> Debug.log "result"
                |> Result.toMaybe
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
