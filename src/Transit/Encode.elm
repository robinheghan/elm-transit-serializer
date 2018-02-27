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
    = TString String
    | TInt Int
    | TBool Bool
    | TList (List Value)
    | TObject (List ( String, Value ))


type alias Cache =
    { counter : Int
    , valueToID : Dict String String
    }


string : String -> Value
string str =
    TString str


int : Int -> Value
int num =
    TInt num


bool : Bool -> Value
bool boolean =
    TBool boolean


list : (a -> Value) -> List a -> Value
list conv ls =
    TList <| List.map conv ls


object : List ( String, Value ) -> Value
object vals =
    TObject vals


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
        TString str ->
            ( cache, JE.string str )

        TInt int ->
            ( cache, JE.int int )

        TBool bool ->
            if bool then
                ( cache, JE.string "?t" )
            else
                ( cache, JE.string "?f" )

        TList list ->
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

        TObject mappings ->
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
