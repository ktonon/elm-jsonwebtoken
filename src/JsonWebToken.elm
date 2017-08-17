module JsonWebToken
    exposing
        ( DecodeError(..)
        , Secret
        , Token
        , decode
        , encode
        , hmacSha1
        , hmacSha256
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

@docs hmacSha1, hmacSha256


## Errors

@docs DecodeError


## Type Aliases

@docs Secret, Token

-}

import Bytes exposing (Bytes)
import Json.Decode exposing (Decoder, decodeString)
import Json.Encode
import JsonWebToken.Base64 as Base64
import JsonWebToken.HMAC as HMAC exposing (HashType(..))
import JsonWebToken.Header as Header exposing (Alg(..), Header)


{-| Verify a token given a secret or public key.

    decode payloadDecoder correctSecret aValidToken
    --> Ok payload

    decode payloadDecoder wrongSecret aValidToken
    --> Err <| InvalidSecret payload

    decode payloadDecoder correctSecret "token.should.have.three.parts"
    --> Err InvalidToken

-}
decode : Decoder a -> Secret -> Token -> Result (DecodeError a) a
decode payloadDecoder secret token =
    case String.split "." token of
        [ part0, part1, signVar ] ->
            decodeHeaderPayload payloadDecoder part0 part1
                |> Result.andThen
                    (verify secret signVar (part0 ++ "." ++ part1))

        _ ->
            Err InvalidToken


{-| Create and sign a token.

    encode hmacSha256 encodePayload correctSecret payload
    --> Ok aValidToken

    encode hmacSha256 Json.Encode.string "other secret" "some payload"
        |> Result.mapError UnexpectedError
        |> Result.andThen (decode Json.Decode.string "other secret")
    --> Ok "some payload"

    encode hmacSha256 Json.Encode.int "123" 456
        |> Result.mapError UnexpectedError
        |> Result.andThen (decode Json.Decode.int "abc")
    --> Err <| InvalidSecret 456

-}
encode : Alg -> (a -> Json.Encode.Value) -> Secret -> a -> Result String Token
encode (HMAC hashType) payloadEncoder secret payload =
    let
        header =
            Header.encode (HMAC SHA256)
                |> Json.Encode.encode 0
                |> Base64.encode

        data =
            payloadEncoder payload
                |> Json.Encode.encode 0
                |> Base64.encode
    in
    sign hashType secret (header ++ "." ++ data)
        |> Result.map
            (\digest ->
                [ header
                , data
                , digest
                ]
                    |> String.join "."
            )


verify : Secret -> String -> String -> ( Header, a ) -> Result (DecodeError a) a
verify key signVar input ( { alg }, payload ) =
    case alg of
        HMAC hash ->
            sign hash key input
                |> Result.mapError UnexpectedError
                |> Result.andThen
                    (\actual ->
                        if actual == signVar then
                            Ok payload
                        else
                            Err <| InvalidSecret payload
                    )



-- ALGORITHMS


{-| HMAC SHA1 digest algorithm
-}
hmacSha1 : Alg
hmacSha1 =
    HMAC SHA1


{-| HMAC SHA256 digest algorithm
-}
hmacSha256 : Alg
hmacSha256 =
    HMAC SHA256



-- ERROR


{-| Types of errors which can occur
-}
type DecodeError a
    = DecodeHeaderFailed String
    | DecodePayloadFailed String
    | InvalidSecret a
    | InvalidToken
    | UnexpectedError String



-- HELPERS


decodeHeaderPayload : Decoder a -> String -> String -> Result (DecodeError a) ( Header, a )
decodeHeaderPayload payloadDecoder header payload =
    Result.map2 (,)
        (Base64.decode header
            |> Result.andThen (decodeString Header.decoder)
            |> Result.mapError DecodeHeaderFailed
        )
        (Base64.decode payload
            |> Result.andThen (decodeString payloadDecoder)
            |> Result.mapError DecodePayloadFailed
        )


sign : HMAC.HashType -> Secret -> String -> Result String String
sign hashType key =
    Bytes.fromUTF8
        >> HMAC.hash hashType (Bytes.fromUTF8 key)
        >> Result.map Base64.encodeHex



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
