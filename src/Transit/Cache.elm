module Transit.Cache
    exposing
        ( WriteCache
        , ReadCache
        , emptyWriteCache
        , emptyReadCache
        , insertWriteCache
        , insertReadCache
        , getFromReadCache
        )

import Array exposing (Array)
import Char
import Dict exposing (Dict)


type alias WriteCache =
    { counter : Int
    , valueToID : Dict String String
    }


type alias ReadCache =
    { counter : Int
    , values : Array String
    }


emptyWriteCache : WriteCache
emptyWriteCache =
    { counter = 0
    , valueToID = Dict.empty
    }


emptyReadCache : ReadCache
emptyReadCache =
    { counter = 0
    , values = Array.empty
    }


insertWriteCache : String -> WriteCache -> ( String, WriteCache )
insertWriteCache key cache =
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


insertReadCache : String -> ReadCache -> ( String, ReadCache )
insertReadCache key cache =
    if String.startsWith "^" key then
        ( Maybe.withDefault key <| Array.get (cacheCodeToCount key) cache.values
        , cache
        )
    else if String.length key > 3 then
        ( key
        , { counter = cache.counter + 1
          , values = Array.push key cache.values
          }
        )
    else
        ( key, cache )


getFromReadCache : String -> ReadCache -> String
getFromReadCache key cache =
    if String.startsWith "^" key then
        Maybe.withDefault key <| Array.get (cacheCodeToCount key) cache.values
    else
        key


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


cacheCodeToCount : String -> Int
cacheCodeToCount str =
    case List.map Char.toCode <| String.toList str of
        [ _, one, two ] ->
            ((one - baseCharIndex) * cacheCodeDigits) + (two - baseCharIndex)

        [ _, one ] ->
            one - baseCharIndex

        _ ->
            0
