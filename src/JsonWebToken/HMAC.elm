module JsonWebToken.HMAC exposing (HashType(..), encodeHashType, hash, hashTypeDecoder)

import Crypto.HMAC exposing (Hash, sha224, sha256, sha384, sha512)
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Encode as Encode


type HashType
    = SHA224
    | SHA256
    | SHA384
    | SHA512


type alias DigestFunction =
    List Int -> List Int -> String


hash : HashType -> Hash
hash hashType =
    case hashType of
        SHA224 ->
            sha224

        SHA256 ->
            sha256

        SHA384 ->
            sha384

        SHA512 ->
            sha512


encodeHashType : HashType -> Encode.Value
encodeHashType hashType =
    Encode.string <|
        case hashType of
            SHA224 ->
                "HS224"

            SHA256 ->
                "HS256"

            SHA384 ->
                "HS384"

            SHA512 ->
                "HS512"


hashTypeDecoder : String -> Decoder HashType
hashTypeDecoder w =
    case String.toLower w of
        "hs224" ->
            Decode.succeed SHA224

        "hs256" ->
            Decode.succeed SHA256

        "hs384" ->
            Decode.succeed SHA384

        "hs512" ->
            Decode.succeed SHA512

        _ ->
            Decode.fail <| "Unsupported algorithm: " ++ w
