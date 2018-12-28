module TestHelpers exposing (Payload, aValidToken, correctSecret, encodePayload, payload, payloadDecoder, wrongSecret)

import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline as Pipeline exposing (required)
import Json.Encode as Encode


aValidToken : String
aValidToken =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0ZXN0IjoicGF5bG9hZCIsImlhdCI6MTUwMjc0MDYyN30=.aRRyq2PDaMD6lsFXJn53cCKPk83LL5hH9k_g71WGzUM="


correctSecret : String
correctSecret =
    "secret"


wrongSecret : String
wrongSecret =
    "wrongSecret"


type alias Payload =
    { test : String
    , iat : Int
    }


payload : Payload
payload =
    Payload "payload" 1502740627


payloadDecoder : Decoder Payload
payloadDecoder =
    Decode.succeed Payload
        |> required "test" Decode.string
        |> required "iat" Decode.int


encodePayload : Payload -> Encode.Value
encodePayload { test, iat } =
    Encode.object
        [ ( "test", Encode.string test )
        , ( "iat", Encode.int iat )
        ]
