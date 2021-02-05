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
import Calc


type alias Model =
    { input : String
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
    ( { input = "1u+ 2o+ 3u+ 1o+ 2u+ 3o+"
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
  let 
      gc = 
        Parser.run Gc.parser model.input

      terms = 
        gc 
        |> Result.toMaybe 
        |> Maybe.andThen Calc.getTerminals
  in
    [ Ht.input [ At.value model.input, Ev.onInput Input ] []
    , Ht.code [] [ Ht.text <| Debug.toString model ]
    , Ht.br [] []
    , Ht.code [] [ Ht.text <| Debug.toString  gc ]
    , Ht.br [] []
    , Ht.code [] [ Ht.text <| Debug.toString terms ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Input s ->
            ( { model | input = s } , Cmd.none)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
