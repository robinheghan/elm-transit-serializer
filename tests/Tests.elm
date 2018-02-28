module Tests exposing (tests)

import Json.Encode as JE
import Transit.Encode as TE
import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Expect


type alias Person =
    { name : String
    , age : Int
    , gender : String
    , isHappy : Bool
    }


samplePersons : List Person
samplePersons =
    [ Person "Robin" 28 "Male" False
    , Person "Evan" 25 "Male" True
    , Person "Johanne" 25 "Female" True
    ]


personEncoder : Person -> TE.Value
personEncoder person =
    TE.object
        [ ( "name", TE.string person.name )
        , ( "age", TE.int person.age )
        , ( "gender", TE.string person.gender )
        , ( "isHappy", TE.bool person.isHappy )
        ]


tests : Test
tests =
    describe "Transit Test"
        [ test "Encodes to JSON arrays with cached keys" <|
            \() ->
                let
                    transit =
                        TE.list personEncoder samplePersons
                            |> TE.encode 0
                in
                    Expect.equal transit <|
                        "[[\"^ \",\"name\",\"Robin\",\"age\",28,\"gender\",\"Male\",\"isHappy\",false],[\"^ \",\"^0\",\"Evan\",\"age\",25,\"^1\",\"Male\",\"^2\",true],[\"^ \",\"^0\",\"Johanne\",\"age\",25,\"^1\",\"Female\",\"^2\",true]]"
        ]
