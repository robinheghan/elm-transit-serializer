module Tests exposing (tests)

import Test exposing (..)
import Fuzz exposing (Fuzzer)
import Expect


tests : Test
tests =
    describe "Transit Test"
        [ test "Failing" <|
            \() ->
                Expect.equal True False
        ]
