module TestHelpers exposing (..)

import Json.Decode as Decode exposing (Decoder, decodeString)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode


aValidToken : String
aValidToken =
    "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ0ZXN0IjoicGF5bG9hZCIsImlhdCI6MTUwMjc0MDYyN30.R6kJENnhgPQ-FJ63otxV8ZHB9usQR-Byt4lKUe8Rw6Y"


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
    decode Payload
        |> required "test" Decode.string
        |> required "iat" Decode.int


encodePayload : Payload -> Encode.Value
encodePayload { test, iat } =
    Encode.object
        [ ( "test", Encode.string test )
        , ( "iat", Encode.int iat )
        ]
