module Tests exposing (tests)

import Json.Encode as JE exposing (Value)
import Transit.Encode as TE
import Transit.Decode as TD
import Test exposing (..)
import Fuzz exposing (Fuzzer)
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
    describe "Transit Test"
        [ test "Encodes to JSON arrays with cached keys" <|
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
        , test "Can be decoded" <|
            \() ->
                TE.list languageEncoder sampleLanguages
                    |> TE.encode 0
                    |> TD.decodeString (TD.list languageDecoder)
                    |> Expect.equal (Ok sampleLanguages)
        ]
