module JsonWebToken
    exposing
        ( Alg
        , DecodeError(..)
        , Secret
        , Token
        , algDecoder
        , decode
        , encode
        , hmacSha224
        , hmacSha256
        , hmacSha384
        , hmacSha512
        )

{-| JSON Web Token encoder and decoder.

Examples below assume the following imports

    import Json.Decode
    import Json.Encode
    import TestHelpers
        exposing
            ( aValidToken
            , correctSecret
            , encodePayload
            , payload
            , payloadDecoder
            , wrongSecret
            )

@docs decode, encode


## Algorithms

@docs hmacSha224, hmacSha256, hmacSha384, hmacSha512


## Errors

@docs DecodeError


## Types

@docs Alg, Secret, Token, algDecoder

-}

import Crypto.HMAC
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode
import JsonWebToken.Base64 as Base64
import JsonWebToken.HMAC as HMAC exposing (HashType(..))


{-| Verify a token given a secret or public key.

If all goes well, you'll get a result back with the payload.

    decode payloadDecoder correctSecret aValidToken
    --> Ok payload

If something goes wrong, you get an error:

    decode payloadDecoder correctSecret "token.should.have.three.parts"
    --> Err InvalidToken

Some errors will include the payload. However, whenever there is an error the
payload should not be trusted.

    decode payloadDecoder wrongSecret aValidToken
    --> Err <| InvalidSecret payload

-}
decode :
    Decoder payload
    -> Secret
    -> Token
    -> Result (DecodeError payload) payload
decode payloadDecoder secret token =
    case String.split "." token of
        [ part0, part1, signVar ] ->
            case decodePayload payloadDecoder part1 of
                Ok payload ->
                    decodeHeader part0
                        |> Result.mapError (DecodeHeaderFailed payload)
                        |> Result.andThen
                            (verify secret signVar payload <| part0 ++ "." ++ part1)

                Err err ->
                    Err <| DecodePayloadFailed err

        _ ->
            Err InvalidToken


{-| Create and sign a token.

    encode hmacSha256 encodePayload correctSecret payload
    --> aValidToken

    encode hmacSha512 Json.Encode.string "other secret" "some payload"
        |> (decode Json.Decode.string "other secret")
    --> Ok "some payload"

    encode hmacSha224 Json.Encode.int "123" 456
        |> (decode Json.Decode.int "abc")
    --> Err <| InvalidSecret 456

-}
encode :
    Alg
    -> (payload -> Encode.Value)
    -> Secret
    -> payload
    -> Token
encode (HMAC hashType) payloadEncoder secret payload =
    let
        header =
            encodeHeader (HMAC hashType)
                |> Encode.encode 0
                |> Base64.encode

        data =
            payloadEncoder payload
                |> Encode.encode 0
                |> Base64.encode
    in
    sign hashType secret (header ++ "." ++ data)
        |> (\digest ->
                [ header
                , data
                , digest
                ]
                    |> String.join "."
           )


verify :
    Secret
    -> String
    -> payload
    -> String
    -> Header
    -> Result (DecodeError payload) payload
verify key signVar payload input { alg } =
    case alg of
        HMAC hash ->
            sign hash key input
                |> (\actual ->
                        if actual == signVar then
                            Ok payload
                        else
                            Err <| InvalidSecret payload
                   )



-- ALGORITHMS


{-| HMAC SHA224 digest algorithm.
-}
hmacSha224 : Alg
hmacSha224 =
    HMAC SHA224


{-| HMAC SHA256 digest algorithm.
-}
hmacSha256 : Alg
hmacSha256 =
    HMAC SHA256


{-| HMAC SHA384 digest algorithm.
-}
hmacSha384 : Alg
hmacSha384 =
    HMAC SHA384


{-| HMAC SHA512 digest algorithm.
-}
hmacSha512 : Alg
hmacSha512 =
    HMAC SHA512



-- ERROR


{-| Types of errors which can occur during decoding of a token.
-}
type DecodeError payload
    = DecodeHeaderFailed payload String
    | DecodePayloadFailed String
    | InvalidSecret payload
    | InvalidToken



-- HELPERS


decodeHeader : String -> Result String Header
decodeHeader header =
    Base64.decode header
        |> Result.andThen (decodeString headerDecoder)


decodePayload : Decoder payload -> String -> Result String payload
decodePayload payloadDecoder payload =
    Base64.decode payload
        |> Result.andThen (decodeString payloadDecoder)


sign : HMAC.HashType -> Secret -> String -> String
sign hashType key message =
    Crypto.HMAC.digest (HMAC.hash hashType) key message
        |> Base64.encodeHex



-- TYPE ALIASES


{-| A JSON web token.
-}
type alias Token =
    String


{-| String used to sign or verify a token.

  - In the case of signing, this can also be a private key.
  - In the case of verifying, this can also be a public key.

-}
type alias Secret =
    String



-- HEADER


type alias Header =
    { typ : Typ
    , alg : Alg
    }


{-| Type of algoirthm to use for the digest
-}
type Alg
    = HMAC HMAC.HashType


type Typ
    = JWT



-- ENCODERS


encodeHeader : Alg -> Encode.Value
encodeHeader (HMAC hashType) =
    Encode.object
        [ ( "alg", HMAC.encodeHashType hashType )
        , ( "typ", Encode.string "JWT" )
        ]



-- DECODERS


headerDecoder : Decoder Header
headerDecoder =
    Pipeline.decode Header
        |> required "typ" typDecoder
        |> required "alg" algDecoder


{-| Algorithm decoder.
-}
algDecoder : Decoder Alg
algDecoder =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen HMAC.hashTypeDecoder
            |> Decode.map HMAC
        ]


typDecoder : Decoder Typ
typDecoder =
    Decode.string
        |> Decode.andThen
            (\w ->
                if w == "JWT" then
                    Decode.succeed JWT
                else
                    Decode.fail "typ is not JWT"
            )
