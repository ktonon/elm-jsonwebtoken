module JsonWebToken.Header exposing (Alg(..), Header, decoder, encode)

import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import JsonWebToken.HMAC as HMAC


type alias Header =
    { typ : Typ
    , alg : Alg
    }


type Alg
    = HMAC HMAC.HashType


type Typ
    = JWT



-- ENCODERS


encode : Alg -> Encode.Value
encode (HMAC hashType) =
    Encode.object
        [ ( "alg", HMAC.encodeHashType hashType )
        , ( "typ", Encode.string "JWT" )
        ]



-- DECODERS


decoder : Decoder Header
decoder =
    decode Header
        |> required "typ" typDecoder
        |> required "alg" algDecoder


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
