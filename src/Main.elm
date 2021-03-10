port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Coknot.Orient
import Coknot.Route
import Css
import ElmEscapeHtml as Esc
import Gc
import Gc.Check
import Gc.Parse
import Html.Styled as Ht
import Html.Styled.Attributes as At
import Html.Styled.Events as Ev
import Json.Decode as Jd
import Json.Encode as Je
import Model
import Parser.Advanced as Parser
import Preset
import Render
import Style
import Svg.Styled as Svg
import Url


main : Program () Model.Model Model.Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = Model.update
        , subscriptions = subscriptions
        , onUrlRequest = \_ -> Model.Nop
        , onUrlChange = \_ -> Model.Nop
        }


init : () -> Url.Url -> Nav.Key -> ( Model.Model, Cmd Model.Msg )
init () url nav =
    ( { nav = nav
      , input =
            { cursor = Nothing
            , content = ""
            }
      , hoverErr = Nothing
      , flat = False
      }
    , Model.setInput "1o+ 2u+ 3o+ 1u+ 2o+ 3u+"
      --, setInput "1o+ 2u+ 3+ 1 2o+ 3u"
    )


port input : (( Maybe Int, String ) -> msg) -> Sub msg


port selection : (Maybe ( Int, Int ) -> msg) -> Sub msg


subscriptions : Model.Model -> Sub Model.Msg
subscriptions model =
    [ input Model.Input
    , selection Model.Selection
    ]
        |> Sub.batch


view : Model.Model -> Browser.Document Model.Msg
view model =
    { title = "hi"
    , body = viewBody model |> List.map Ht.toUnstyled
    }


viewBody : Model.Model -> List (Ht.Html Model.Msg)
viewBody model =
    let
        gc =
            Gc.Parse.gaussCode model.input.content

        parseErrs =
            case gc of
                Ok _ ->
                    []

                Err es ->
                    es

        errs =
            gc
                |> Result.toMaybe
                |> Maybe.map Gc.Check.check

        terms =
            gc
                |> Result.toMaybe
                |> Maybe.andThen Coknot.Orient.getTerminals

        layout =
            terms
                |> Maybe.andThen Coknot.Route.build
    in
    [ Ht.main_ [ At.css [ Style.root ] ]
        [ Ht.div [ At.css [ Style.content ] ]
            [ Ht.div
                [ At.css [ Style.controls ] ]
                [ Ht.div
                    [ At.css [ Style.examples ] ]
                    (Ht.label [ At.css [ Style.examplesLabel ] ]
                        [ Ht.text "Try some examples:" ]
                        :: List.map
                            (\ex ->
                                Ht.button
                                    [ At.css [ Style.example ]
                                    , Ev.onClick <| Model.SetInput ex.code
                                    ]
                                    ex.name
                            )
                            Preset.all
                    )
                , Ht.div [ At.css [ Style.editor ] ]
                    [ Ht.button
                        [ At.css [ Style.btn ]
                        , Ev.onClick Model.RotLeft
                        ]
                        [ Ht.text <| Esc.unescape "&#x21b6;" ]
                    , Ht.div [ At.css [ Style.inputWrapper ] ]
                        [ Ht.textarea
                            [ At.id "gauss"
                            , At.css [ Style.input ]
                            , At.rows 1
                            , Ev.onBlur (Model.Selection Nothing)
                            , At.placeholder "Enter a Gauss code (e.g. `1o+ 1u+`)..."
                            ]
                            []
                        , Ht.div
                            [ At.css
                                [ Style.carets ]
                            ]
                            (viewErrCarets model parseErrs)
                        ]
                    , Ht.button
                        [ At.css [ Style.btn ]
                        , Ev.onClick Model.RotRight
                        ]
                        [ Ht.text <| Esc.unescape "&#x21b7;" ]
                    ]
                , Ht.div [ At.css [ Style.msg ] ]
                    (parseErrs
                        |> List.filter
                            (\e ->
                                model.input.cursor
                                    |> Maybe.map (\c -> e.pos <= c && c <= e.pos + e.len)
                                    |> Maybe.withDefault False
                            )
                        |> List.map viewErrMsg
                    )
                , Ht.div []
                    [ Ht.input
                        [ At.type_ "checkbox"
                        , At.checked model.flat
                        , Ev.onClick Model.ToggleFlat
                        ]
                        []
                    ]
                ]
            , Ht.div [ At.css [ Style.diagramWrapper ] ] <|
                case layout of
                    Nothing ->
                        []

                    Just l ->
                        [ Render.render model.flat l ]
            ]
        ]
    ]


viewErrCarets : Model.Model -> List Gc.Parse.Error -> List (Ht.Html Model.Msg)
viewErrCarets model =
    List.foldl
        (\e state ->
            { els =
                Ht.span
                    [ At.css [ Style.caret ]
                    , Ev.onMouseOver <| Model.HoverIn e.index
                    , Ev.onMouseOut Model.HoverOut
                    ]
                    [ Ht.text <| String.repeat e.len " " ]
                    :: (Ht.text <| String.repeat (e.pos - state.pos) " ")
                    :: state.els
            , pos = e.pos + e.len
            }
        )
        { pos = 0
        , els = []
        }
        >> (.els >> List.reverse)


ordinalToString : Int -> String
ordinalToString n =
    String.fromInt (n + 1)
        ++ (case n of
                0 ->
                    "st"

                1 ->
                    "nd"

                2 ->
                    "rd"

                _ ->
                    "th"
           )


viewErrMsg : Gc.Parse.Error -> Ht.Html Model.Msg
viewErrMsg err =
    Ht.div [ At.css [ Style.err ] ]
        [ Ht.span
            []
            [ Ht.text "Syntax error: " ]
        , Ht.span
            []
            (let
                code =
                    Ht.code [ At.css [ Style.code ] ] << List.singleton << Ht.text

                codec =
                    code << String.fromChar
             in
             case err.err of
                Gc.Parse.ExpectingLabel ->
                    [ Ht.text "Expecting to see a label (e.g., "
                    , code "5"
                    , Ht.text " or "
                    , code "12"
                    , Ht.text ")"
                    ]

                Gc.Parse.ExpectingOrder c ->
                    if c == '+' || c == '-' then
                        [ Ht.text "Need to specify "
                        , codec 'u'
                        , Ht.text " (under) or "
                        , codec 'o'
                        , Ht.text " (over) before the sign "
                        , codec c
                        , Ht.text "."
                        ]

                    else
                        [ Ht.text "Expecting to see `u` (under) or `o` (over); got `"
                        , codec c
                        , Ht.text " instead."
                        ]

                Gc.Parse.ExpectingSign c ->
                    [ Ht.text
                        "Expecting to see a sign, i.e. "
                    , codec '+'
                    , Ht.text " or "
                    , codec '-'
                    , Ht.text "; got "
                    , codec c
                    , Ht.text " instead."
                    ]

                Gc.Parse.MissingSign ->
                    [ Ht.text "Need to specify a sign, i.e. "
                    , codec '+'
                    , Ht.text " or "
                    , codec '-'
                    , Ht.text "."
                    ]

                Gc.Parse.MissingOrder ->
                    [ Ht.text "Need to specify "
                    , codec 'u'
                    , Ht.text " (under) or "
                    , codec 'o'
                    , Ht.text " (over)."
                    ]

                Gc.Parse.ExpectingEnd ->
                    [ Ht.text "Too many characters. Did you accidentally miss a space?" ]
            )
        ]
