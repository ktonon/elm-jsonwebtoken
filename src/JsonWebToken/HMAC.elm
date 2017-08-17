module JsonWebToken.HMAC exposing (HashType(..), encodeHashType, hash, hashTypeDecoder)

import Bytes exposing (Bytes)
import HMAC
import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Encode as Encode


type HashType
    = SHA1
    | SHA256


type alias DigestFunction =
    Bytes -> Bytes -> Result String String


hash : HashType -> DigestFunction
hash hashType =
    case hashType of
        SHA1 ->
            HMAC.sha1

        SHA256 ->
            HMAC.sha256


encodeHashType : HashType -> Encode.Value
encodeHashType hashType =
    Encode.string <|
        case hashType of
            SHA1 ->
                "HS1"

            SHA256 ->
                "HS256"


hashTypeDecoder : String -> Decoder HashType
hashTypeDecoder w =
    case String.toLower w of
        "hs1" ->
            Decode.succeed <| SHA1

        "hs256" ->
            Decode.succeed <| SHA256

        _ ->
            Decode.fail <| "Unsupported algorithm: " ++ w
