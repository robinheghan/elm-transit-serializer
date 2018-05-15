module Tests exposing (tests)

import Transit.Encode as TE
import Transit.Decode as TD
import Test exposing (..)
import Expect


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
                \() ->
                    let
                        transit =
                            TE.list languageEncoder sampleLanguages
                                |> TE.encode 0
                    in
                        Expect.equal transit <|
                            ("[[\"^ \",\"name\",\"Elm\",\"age\",5,\"syntaxInspiration\",\"ML\",\"isStaticTyped\",true],"
                                ++ "[\"^ \",\"^0\",\"Clojure\",\"age\",10,\"^1\",\"Lisp\",\"^2\",false],"
                                ++ "[\"^ \",\"^0\",\"Go\",\"age\",10,\"^1\",\"C\",\"^2\",true]]"
                            )
            , test "Decoded" <|
                \() ->
                    TE.list languageEncoder sampleLanguages
                        |> TE.encode 0
                        |> TD.decodeString (TD.list languageDecoder)
                        |> Expect.equal (Ok sampleLanguages)
            ]
        , describe "Keywords"
            [ test "Encode" <|
                \() ->
                    let
                        transit =
                            TE.list TE.keyword [ "test", "test" ]
                                |> TE.encode 0
                    in
                        Expect.equal transit <|
                            "[\"~:test\",\"^0\"]"
            , test "Decode" <|
                \() ->
                    TE.list TE.keyword [ "test", "test" ]
                        |> TE.encode 0
                        |> TD.decodeString (TD.list TD.keyword)
                        |> Expect.equal (Ok [ "test", "test" ])
            ]
        ]
