port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Calc
import Gc
import Gc.Check
import Gc.Parse
import Html.Styled as Ht
import Html.Styled.Attributes as At
import Html.Styled.Events as Ev
import Json.Decode as Jd
import Json.Encode as Je
import Parser.Advanced as Parser
import Style
import Url


type alias Model =
    { nav : Nav.Key
    , input :
        { cursor : Int
        , content : String
        }
    }


type Msg
    = Nop
    | Input ( Int, String )
    | Selection ( Int, Int )


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


port setInput : String -> Cmd msg


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init () url nav =
    ( { nav = nav
      , input =
            { cursor = 0
            , content = ""
            }
      }
    , setInput "1o+ 2u+ 3o+ 1u+ 2o+ 3u+"
    )


port input : (( Int, String ) -> msg) -> Sub msg


port selection : (( Int, Int ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    [ input Input
    , selection Selection
    ]
        |> Sub.batch


view : Model -> Browser.Document Msg
view model =
    { title = "hi"
    , body = viewBody model |> List.map Ht.toUnstyled
    }


viewBody : Model -> List (Ht.Html Msg)
viewBody model =
    let
        gc =
            Gc.Parse.gaussCode model.input.content

        errs =
            gc
                |> Result.toMaybe
                |> Maybe.map Gc.Check.check

        terms =
            gc
                |> Result.toMaybe
                |> Maybe.andThen Calc.getTerminals
    in
    [ Ht.main_ [ At.css [ Style.root ] ]
        [ Ht.div [ At.css [ Style.content ] ]
            [ Ht.input
                [ At.id "gauss"
                , At.css [ Style.input ]
                ]
                []
            , Ht.br [] []
            , Ht.code [] [ Ht.text <| Debug.toString gc ]
            , Ht.br [] []
            , Ht.code [] [ Ht.text <| Debug.toString errs ]
            , Ht.br [] []
            , Ht.code [] [ Ht.text <| Debug.toString terms ]
            ]
        ]
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Nop ->
            ( model, Cmd.none )

        Input ( i, content ) ->
            ( { model
                | input = { cursor = i, content = content }
              }
            , Cmd.none
            )

        Selection ( i, _ ) ->
            model.input
                |> (\data ->
                        ( { model
                            | input =
                                { data | cursor = i }
                          }
                        , Cmd.none
                        )
                   )
