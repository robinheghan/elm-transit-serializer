module Transit.Encode
    exposing
        ( Value
        , encode
        , string
        , int
        , bool
        , list
        , object
        )

import Char
import Dict exposing (Dict)
import Json.Encode as JE


type Value
    = Str String
    | Integer Int
    | Boolean Bool
    | Ls (List Value)
    | Obj (List ( String, Value ))


type alias Cache =
    { counter : Int
    , valueToID : Dict String String
    }


string : String -> Value
string str =
    Str str


int : Int -> Value
int num =
    Integer num


bool : Bool -> Value
bool boolean =
    Boolean boolean


list : (a -> Value) -> List a -> Value
list conv ls =
    List.map conv ls
        |> Ls


object : List ( String, Value ) -> Value
object vals =
    Obj vals


encode : Int -> Value -> String
encode indent val =
    let
        ( _, json ) =
            valueToJSON emptyCache val
    in
        JE.encode indent json


emptyCache : Cache
emptyCache =
    { counter = 0
    , valueToID = Dict.empty
    }


valueToJSON : Cache -> Value -> ( Cache, JE.Value )
valueToJSON cache val =
    case val of
        Str str ->
            ( cache, JE.string str )

        Integer int ->
            ( cache, JE.int int )

        Boolean bool ->
            if bool then
                ( cache, JE.string "?t" )
            else
                ( cache, JE.string "?f" )

        Ls list ->
            let
                helper val ( currCache, acc ) =
                    let
                        ( nextCache, jsonified ) =
                            valueToJSON currCache val
                    in
                        ( nextCache, jsonified :: acc )
            in
                List.foldl helper ( cache, [] ) list
                    |> Tuple.mapSecond (JE.list << List.reverse)

        Obj mappings ->
            let
                helper ( key, val ) ( currCache, acc ) =
                    let
                        ( cacheWithKey, cachedKey ) =
                            cacheKey currCache key

                        ( nextCache, jsonified ) =
                            valueToJSON cacheWithKey val
                    in
                        ( nextCache, jsonified :: JE.string cachedKey :: acc )
            in
                List.foldl helper ( cache, [ JE.string "^ " ] ) mappings
                    |> Tuple.mapSecond (JE.list << List.reverse)


cacheKey : Cache -> String -> ( Cache, String )
cacheKey cache key =
    if String.length key > 3 then
        case Dict.get key cache.valueToID of
            Just cacheID ->
                ( cache, cacheID )

            Nothing ->
                let
                    idifiedKey =
                        countToCacheCode cache.counter
                in
                    ( { counter = cache.counter + 1
                      , valueToID = Dict.insert key idifiedKey cache.valueToID
                      }
                    , key
                    )
    else
        ( cache, key )


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
