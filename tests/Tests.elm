module Tests exposing (tests)

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
        [ test "Failing" <|
            \() ->
                TE.list personEncoder
                    [ Person "Robin" 28 "Male" False
                    , Person "Evan" 25 "Male" True
                    , Person "Johanne" 25 "Female" True
                    ]
                    |> TE.encode 0
                    |> toString
                    |> Expect.equal ""
        ]
