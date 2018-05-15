module Transit.Encode
    exposing
        ( Value
        , string
        , keyword
        , int
        , bool
        , list
        , object
        , encode
        )

import Transit.Cache as Cache exposing (WriteCache)
import Json.Encode as JE


type Value
    = TString String
    | TKeyword String
    | TInt Int
    | TBool Bool
    | TList (List Value)
    | TObject (List ( String, Value ))


string : String -> Value
string str =
    TString str


keyword : String -> Value
keyword str =
    TKeyword str


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
            valueToJSON Cache.emptyWriteCache val
    in
        JE.encode indent json


valueToJSON : WriteCache -> Value -> ( WriteCache, JE.Value )
valueToJSON cache val =
    case val of
        TString str ->
            ( cache, JE.string str )

        TKeyword str ->
            let
                encoded =
                    "~:" ++ str

                ( cachedKey, cacheWithKey ) =
                    Cache.insertWriteCache encoded cache
            in
                ( cacheWithKey, JE.string cachedKey )

        TInt int ->
            ( cache, JE.int int )

        TBool bool ->
            ( cache, JE.bool bool )

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
                        ( cachedKey, cacheWithKey ) =
                            Cache.insertWriteCache key currCache

                        ( nextCache, jsonified ) =
                            valueToJSON cacheWithKey val
                    in
                        ( nextCache, jsonified :: JE.string cachedKey :: acc )
            in
                List.foldl helper ( cache, [ JE.string "^ " ] ) mappings
                    |> Tuple.mapSecond (JE.list << List.reverse)
