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
import Parser.Advanced as Parser
import Preset
import Render
import Style
import Svg.Styled as Svg
import Url


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
            { cursor = Nothing
            , content = ""
            }
      , hoverErr = Nothing
      }
    , setInput "1o+ 2u+ 3o+ 1u+ 2o+ 3u+"
      --, setInput "1o+ 2u+ 3+ 1 2o+ 3u"
    )


port input : (( Maybe Int, String ) -> msg) -> Sub msg


port selection : (Maybe ( Int, Int ) -> msg) -> Sub msg


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


addHighlight : Model -> Gc.Parse.Error -> (List Css.Style -> List Css.Style)
addHighlight model err =
    if
        (model.hoverErr == Just err.index)
            || (model.input.cursor
                    |> Maybe.map
                        (\c ->
                            (err.pos + err.char - 1 <= c)
                                && (c <= err.pos + err.char + 1)
                        )
                    |> Maybe.withDefault False
               )
    then
        (::) Style.highlight

    else
        identity


viewBody : Model -> List (Ht.Html Msg)
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
                                    , Ev.onClick <| SetInput ex.code
                                    ]
                                    ex.name
                            )
                            Preset.all
                    )
                , Ht.textarea
                    [ At.id "gauss"
                    , At.css [ Style.input ]
                    , At.rows 1
                    , Ev.onBlur (Selection Nothing)
                    , At.placeholder "Enter a Gauss code (e.g. `1o+ 1u+`)..."
                    ]
                    []
                , Ht.div
                    [ At.css [ Style.carets ] ]
                    (viewErrCarets model parseErrs)
                , Ht.dl
                    [ At.css [ Style.errs ] ]
                    (viewErrMsgs model parseErrs)
                ]
            , Ht.div [] <|
                case layout of
                    Nothing ->
                        []

                    Just l ->
                        [ Render.render l ]
            , Ht.br [] []
            , Ht.code [] [ Ht.text <| Debug.toString gc ]
            , Ht.br [] []
            , Ht.code [] [ Ht.text <| Debug.toString errs ]
            , Ht.br [] []
            , Ht.code [] [ Ht.text <| Debug.toString terms ]
            , Ht.br [] []
            , Ht.code [] [ Ht.text <| Debug.toString layout ]
            ]
        ]
    ]


viewErrCarets : Model -> List Gc.Parse.Error -> List (Ht.Html Msg)
viewErrCarets model =
    List.foldl
        (\e state ->
            { els =
                Ht.span
                    [ At.css <| addHighlight model e [ Style.caret ]
                    , Ev.onMouseOver <| HoverIn e.index
                    , Ev.onMouseOut HoverOut
                    ]
                    [ Ht.text "^^" ]
                    :: (Ht.text <| String.repeat (e.pos + e.char - state.pos - 1) " ")
                    :: state.els
            , pos = e.pos + e.char + 1
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


viewErrMsgs : Model -> List Gc.Parse.Error -> List (Ht.Html Msg)
viewErrMsgs model =
    List.concatMap
        (\err ->
            [ Ht.dt
                [ At.css <| addHighlight model err [ Style.errLabel ]
                , Ev.onMouseOver <| HoverIn err.index
                , Ev.onMouseOut HoverOut
                ]
                [ Ht.text <| "Syntax error at " ++ ordinalToString err.index ++ " waypoint"
                ]
            , Ht.dd
                [ At.css <| addHighlight model err [ Style.errMsg ]
                , Ev.onMouseOver <| HoverIn err.index
                , Ev.onMouseOut HoverOut
                ]
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
        )


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

        SetInput s ->
            ( model, setInput s )

        Selection sel ->
            model.input
                |> (\data ->
                        ( { model
                            | input =
                                { data | cursor = Maybe.map Tuple.first sel }
                          }
                        , Cmd.none
                        )
                   )

        HoverIn i ->
            ( { model | hoverErr = Just i }, Cmd.none )

        HoverOut ->
            ( { model | hoverErr = Nothing }, Cmd.none )
