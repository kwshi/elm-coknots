module GaussCode.Parser exposing
    ( Bad(..)
    , Parser
    , crossing
    , gaussCode
    )

import GaussCode.Common exposing (..)
import Parser.Advanced as Parser exposing ((|.))


type Bad
    = ShouldNeverHappen
    | ExpectingSign
    | ExpectingOrder
    | ExpectingDigit
    | ExpectingEnd
    | ExpectingDelim


type alias Parser c a =
    Parser.Parser c Bad a


gaussCode : Parser c GaussCode
gaussCode =
    Parser.loop ( False, [] ) <|
        \( delim, crossings ) ->
            Parser.oneOf
                [ if delim then
                    Parser.succeed (Parser.Loop ( False, crossings ))
                        |. Parser.chompIf ((==) ' ') ExpectingDelim
                        |. Parser.chompWhile ((==) ' ')

                  else
                    crossing
                        |> Parser.map (\c -> Parser.Loop ( True, c :: crossings ))
                , Parser.succeed
                    (Parser.Done <| List.reverse crossings)
                    |. Parser.end ExpectingEnd
                ]


crossing : Parser.Parser ctx Bad Crossing
crossing =
    let
        parseIfMissing current p f =
            case current of
                Nothing ->
                    Just (Parser.map (Just >> f) p)

                Just _ ->
                    Nothing
    in
    Parser.loop
        { label = Nothing, order = Nothing, sign = Nothing }
    <|
        \c ->
            [ parseIfMissing c.label label <| \l -> { c | label = l }
            , parseIfMissing c.order order <| \o -> { c | order = o }
            , parseIfMissing c.sign sign <| \s -> { c | sign = s }
            ]
                |> List.filterMap identity
                |> Parser.oneOf
                |> Parser.map
                    (\cNew ->
                        case ( cNew.label, cNew.order, cNew.sign ) of
                            ( Just l, Just o, Just s ) ->
                                Parser.Done (Crossing l o s)

                            _ ->
                                Parser.Loop cNew
                    )


label : Parser c Int
label =
    Parser.chompIf Char.isDigit ExpectingDigit
        |. Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen
            (\s ->
                case String.toInt s of
                    Nothing ->
                        Parser.problem ShouldNeverHappen

                    Just n ->
                        Parser.succeed n
            )


enum : Bad -> List ( String, a ) -> Parser c a
enum bad =
    List.map
        (\( k, v ) ->
            Parser.succeed v
                |. Parser.token (Parser.Token k bad)
        )
        >> Parser.oneOf


sign : Parser c Sign
sign =
    enum ExpectingSign [ ( "+", Plus ), ( "-", Minus ) ]


order : Parser c GaussCode.Common.Order
order =
    enum ExpectingOrder [ ( "o", Over ), ( "u", Under ) ]
