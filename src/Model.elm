port module Model exposing (..)

import Browser.Navigation as Nav
import Gc.Parse


port setInput : String -> Cmd msg


port setSelection : ( Int, Int ) -> Cmd msg


port input : (( Maybe Int, String ) -> msg) -> Sub msg


port selection : (Maybe ( Int, Int ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    [ input Input
    , selection Selection
    ]
        |> Sub.batch


type alias Model =
    { nav : Nav.Key
    , input :
        { cursor : Maybe Int
        , content : String
        }
    , hoverErr : Maybe Int
    , flat : Bool
    }


type CycleDir
    = Prev
    | Next


type Msg
    = Nop
    | Input ( Maybe Int, String )
    | InputSkip CycleDir
    | Selection (Maybe ( Int, Int ))
    | SetInput String
    | HoverIn Int
    | HoverOut
    | RotLeft
    | RotRight
    | ToggleFlat


parts : String -> List String
parts =
    String.toList
        >> Gc.Parse.stripSplit ((==) ' ')
        >> List.map (.run >> String.fromList)


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

        InputSkip dir ->
            ( model
            , setSelection ( 0, 0 )
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

        RotLeft ->
            ( model, setInput (parts model.input.content |> rotLeft |> String.join " ") )

        RotRight ->
            ( model, setInput (parts model.input.content |> rotRight |> String.join " ") )

        ToggleFlat ->
            ( { model | flat = not model.flat }, Cmd.none )


rotLeft : List a -> List a
rotLeft l =
    case l of
        [] ->
            []

        a :: rest ->
            rest ++ [ a ]


rotRight : List a -> List a
rotRight =
    List.reverse >> rotLeft >> List.reverse
