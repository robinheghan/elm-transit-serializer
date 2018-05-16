module Tests exposing (tests)

import Transit.Encode as TE
import Transit.Decode as TD
import Test exposing (..)
import Expect
import String


type alias Language =
    { name : String
    , age : Int
    , syntaxInspiration : String
    , isStaticTyped : Bool
    }


sampleLanguages : List Language
sampleLanguages =
    [ Language "Elm" 5 "ML" True
    , Language "Clojure" 10 "Lisp" False
    , Language "Go" 10 "C" True
    ]


languageEncoder : Language -> TE.Value
languageEncoder language =
    TE.object
        [ ( "name", TE.string language.name )
        , ( "age", TE.int language.age )
        , ( "syntaxInspiration", TE.string language.syntaxInspiration )
        , ( "isStaticTyped", TE.bool language.isStaticTyped )
        ]


languageDecoder : TD.Decoder Language
languageDecoder =
    TD.map4 Language
        (TD.field "name" TD.string)
        (TD.field "age" TD.int)
        (TD.field "syntaxInspiration" TD.string)
        (TD.field "isStaticTyped" TD.bool)


tests : Test
tests =
    describe "Transit"
        [ describe "Lists and Records"
            [ test "Encode with cached keys" <|
                \_ ->
                    sampleLanguages
                        |> TE.list languageEncoder
                        |> TE.encode 0
                        |> Expect.equal
                            ("[[\"^ \",\"name\",\"Elm\",\"age\",5,\"syntaxInspiration\",\"ML\",\"isStaticTyped\",true],"
                                ++ "[\"^ \",\"^0\",\"Clojure\",\"age\",10,\"^1\",\"Lisp\",\"^2\",false],"
                                ++ "[\"^ \",\"^0\",\"Go\",\"age\",10,\"^1\",\"C\",\"^2\",true]]"
                            )
            , test "Decoded" <|
                \_ ->
                    sampleLanguages
                        |> TE.list languageEncoder
                        |> TE.encode 0
                        |> TD.decodeString (TD.list languageDecoder)
                        |> Expect.equal (Ok sampleLanguages)
            ]
        , describe "Keywords and Symbols" <|
            let
                encodeTest encoder =
                    \_ ->
                        [ "test", "test" ]
                            |> TE.list TE.keyword
                            |> TE.encode 0
                            |> Expect.equal "[\"~:test\",\"^0\"]"

                decodeTest encoder decoder =
                    \_ ->
                        [ "test", "test" ]
                            |> TE.list encoder
                            |> TE.encode 0
                            |> TD.decodeString (TD.list decoder)
                            |> Expect.equal (Ok [ "test", "test" ])
            in
                [ test "Encode Keyword" (encodeTest TE.keyword)
                , test "Encode Symbol" (encodeTest TE.symbol)
                , test "Decode Keyword" (decodeTest TE.keyword TD.keyword)
                , test "Decode Symbol" (decodeTest TE.symbol TD.symbol)
                ]
        , describe "Cache" <|
            let
                duplicatePairs acc ls =
                    case ls of
                        [] ->
                            acc

                        x :: xs ->
                            duplicatePairs (x :: x :: acc) xs

                sample =
                    List.range 0 (44 * 44 + 10)
                        |> List.map (\i -> "key-" ++ toString i)
                        |> duplicatePairs []

                equalEnd expected total =
                    total
                        |> String.right (String.length expected)
                        |> Expect.equal expected
            in
                [ test "Encode: Max cache size is 44^2, and should reset on overflow" <|
                    \_ ->
                        sample
                            |> TE.list TE.keyword
                            |> TE.encode 0
                            |> equalEnd "\"~:key-1\",\"^9\",\"~:key-0\",\"^:\"]"
                , test "Decode: Max cache size is 44^2, and should reset on overflow" <|
                    \_ ->
                        sample
                            |> TE.list TE.keyword
                            |> TE.encode 0
                            |> TD.decodeString (TD.list TD.keyword)
                            |> Expect.equal (Ok sample)
                ]
        ]
