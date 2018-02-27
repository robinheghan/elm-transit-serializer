module Transit.Cache
    exposing
        ( Cache
        , empty
        , insertKey
        )

import Char
import Dict exposing (Dict)


type alias Cache =
    { counter : Int
    , valueToID : Dict String String
    }


empty : Cache
empty =
    { counter = 0
    , valueToID = Dict.empty
    }


insertKey : String -> Cache -> ( String, Cache )
insertKey key cache =
    if String.length key > 3 then
        case Dict.get key cache.valueToID of
            Just cacheID ->
                ( cacheID, cache )

            Nothing ->
                let
                    code =
                        countToCacheCode cache.counter
                in
                    ( key
                    , { counter = cache.counter + 1
                      , valueToID = Dict.insert key code cache.valueToID
                      }
                    )
    else
        ( key, cache )


cacheCodeDigits : Int
cacheCodeDigits =
    44


baseCharIndex : Int
baseCharIndex =
    48


subStr : Char
subStr =
    '^'


countToCacheCode : Int -> String
countToCacheCode count =
    let
        hi =
            count // cacheCodeDigits

        lo =
            count % cacheCodeDigits
    in
        if hi == 0 then
            String.fromList
                [ subStr
                , Char.fromCode (lo + baseCharIndex)
                ]
        else
            String.fromList
                [ subStr
                , Char.fromCode (hi + baseCharIndex)
                , Char.fromCode (lo + baseCharIndex)
                ]
