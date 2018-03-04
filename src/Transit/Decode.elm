module Transit.Decode
    exposing
        ( Decoder
        , string
        , bool
        , int
        , list
        , map4
        , field
        , decodeString
        )

import Dict exposing (Dict)
import Json.Encode as JE exposing (Value)
import Json.Decode as JD
import Transit.Cache as Cache exposing (ReadCache)


type Decoder a
    = Decoder (Value -> ReadCache -> ( ReadCache, Result String a ))


type alias DecodedObj =
    Dict String Value


string : Decoder String
string =
    let
        decoder value cache =
            case JD.decodeValue JD.string value of
                Ok str ->
                    ( cache, Ok str )

                Err _ ->
                    ( cache, Err "Not a string" )
    in
        Decoder decoder


bool : Decoder Bool
bool =
    let
        decoder value cache =
            case JD.decodeValue JD.bool value of
                Ok bool ->
                    ( cache, Ok bool )

                Err _ ->
                    ( cache, Err "Not a boolean" )
    in
        Decoder decoder


int : Decoder Int
int =
    let
        decoder value cache =
            case JD.decodeValue JD.int value of
                Ok int ->
                    ( cache, Ok int )

                Err _ ->
                    ( cache, Err "Not an int" )
    in
        Decoder decoder


list : Decoder a -> Decoder (List a)
list (Decoder contentDecoder) =
    let
        decoder : Value -> ReadCache -> ( ReadCache, Result String (List a) )
        decoder value cache =
            case JD.decodeValue (JD.list JD.value) value of
                Ok ls ->
                    List.foldl foldHelper ( cache, Ok [] ) ls
                        |> Tuple.mapSecond (Result.map List.reverse)

                Err _ ->
                    ( cache, Err "Not a list" )

        foldHelper :
            Value
            -> ( ReadCache, Result String (List a) )
            -> ( ReadCache, Result String (List a) )
        foldHelper val (( cache, res ) as acc) =
            case res of
                Ok ls ->
                    case contentDecoder val cache of
                        ( updatedCache, Ok decoded ) ->
                            ( updatedCache, Ok <| decoded :: ls )

                        ( updatedCache, Err error ) ->
                            ( updatedCache, Err error )

                Err _ ->
                    acc
    in
        Decoder decoder


map4 :
    (a -> b -> c -> d -> e)
    -> (DecodedObj -> ReadCache -> ( ReadCache, Result String a ))
    -> (DecodedObj -> ReadCache -> ( ReadCache, Result String b ))
    -> (DecodedObj -> ReadCache -> ( ReadCache, Result String c ))
    -> (DecodedObj -> ReadCache -> ( ReadCache, Result String d ))
    -> Decoder e
map4 ctor one two three four =
    let
        decoder value cache =
            case decodeObject cache value of
                ( cacheAfterDecode, Ok decoded ) ->
                    let
                        ( cache1, firstParam ) =
                            (one decoded cacheAfterDecode)

                        ( cache2, secondParam ) =
                            (two decoded cache1)

                        ( cache3, thirdParam ) =
                            (three decoded cache2)

                        ( cache4, fourthParam ) =
                            (four decoded cache3)
                    in
                        case ( firstParam, secondParam, thirdParam, fourthParam ) of
                            ( Ok first, Ok second, Ok third, Ok fourth ) ->
                                ( cache4, Ok <| ctor first second third fourth )

                            _ ->
                                ( cache4, Err "Decoding failed" )

                ( cacheAfterDecode, Err error ) ->
                    ( cacheAfterDecode, Err error )
    in
        Decoder decoder


decodeObject : ReadCache -> Value -> ( ReadCache, Result String DecodedObj )
decodeObject cache value =
    let
        readMapID : List Value -> Result String (List Value)
        readMapID list =
            case list of
                first :: rest ->
                    case JD.decodeValue JD.string first of
                        Ok "^ " ->
                            Ok rest

                        _ ->
                            Err "Not a valid map"

                [] ->
                    Err "Not a valid map"

        keyValueDecode :
            List Value
            -> ( ReadCache, Dict String Value )
            -> ( ReadCache, Dict String Value )
        keyValueDecode list (( cache, decoded ) as acc) =
            case list of
                key :: value :: rest ->
                    case JD.decodeValue JD.string key of
                        Ok strKey ->
                            let
                                ( cachedKey, updatedCache ) =
                                    Cache.insertReadCache strKey cache
                            in
                                keyValueDecode
                                    rest
                                    ( updatedCache, Dict.insert cachedKey value decoded )

                        Err _ ->
                            acc

                invalid :: rest ->
                    acc

                [] ->
                    acc
    in
        case JD.decodeValue (JD.list JD.value) value of
            Ok ls ->
                case readMapID ls of
                    Ok keyValues ->
                        let
                            ( updatedCache, decoded ) =
                                keyValueDecode keyValues ( cache, Dict.empty )
                        in
                            ( updatedCache
                            , Ok <| decoded
                            )

                    Err err ->
                        ( cache, Err err )

            Err _ ->
                ( cache, Err "Not a map" )


isError : Result a b -> Bool
isError result =
    case result of
        Ok _ ->
            False

        Err _ ->
            True


field : String -> Decoder a -> (DecodedObj -> ReadCache -> ( ReadCache, Result String a ))
field propName (Decoder fn) =
    (\decoded cache ->
        case Dict.get propName decoded of
            Just value ->
                let
                    ( updatedCache, result ) =
                        fn value cache
                in
                    ( updatedCache, result )

            Nothing ->
                ( cache, Err "Failed to find value" )
    )


decodeString : Decoder a -> String -> Result String a
decodeString (Decoder fn) str =
    case JD.decodeString JD.value str of
        Ok val ->
            fn val Cache.emptyReadCache
                |> Tuple.second

        Err error ->
            Err error
