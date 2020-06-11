module JsonWebTokenTests exposing (all)

import Base64
import Expect
import Json.Decode
import Json.Encode
import JsonWebToken exposing (DecodeError(..), decode)
import Test exposing (Test, describe, test)
import Word.Bytes as Bytes


all : Test
all =
    describe "JsonWebToken.verify"
        [ test "verify token with too few parts" <|
            \_ ->
                Expect.equal
                    (Err InvalidToken)
                    (decode examplePayloadDecoder exampleSecret "foo.bar")
        , test "verify token with too many parts" <|
            \_ ->
                Expect.equal
                    (Err InvalidToken)
                    (decode examplePayloadDecoder exampleSecret "foo.bar.car.far")
        , test "verify token with valid secret" <|
            \_ ->
                Expect.equal
                    (Ok examplePayload)
                    (decode examplePayloadDecoder exampleSecret exampleToken)
        , test "decode a token without a typ in the header" <|
            \_ ->
                Expect.equal
                    (Ok examplePayload)
                    (decode examplePayloadDecoder exampleSecret exampleTokenWithoutTypHeader)
        , test "encode a token without including base64-padding" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        Expect.true ("expected base64-encoded payload '" ++ base64EncodedExamplePayload ++ "' to include some '=' padding-symbols")
                            (String.contains "=" base64EncodedExamplePayload)
                    , \_ ->
                        Expect.false ("expected jwt-encoded payload '" ++ base64EncodedExamplePayload ++ "' to not include any '=' padding-symbols")
                            (String.contains "=" exampleToken)
                    ]
                    ()
        ]



-- Example


exampleSecret : String
exampleSecret =
    "secret"


examplePayload : { key : String }
examplePayload =
    { key = "values" }


encodeExamplePayload : { key : String } -> Json.Encode.Value
encodeExamplePayload { key } =
    Json.Encode.object [ ( "key", Json.Encode.string key ) ]


examplePayloadDecoder : Json.Decode.Decoder { key : String }
examplePayloadDecoder =
    Json.Decode.map (\value -> { key = value })
        (Json.Decode.field "key" Json.Decode.string)


base64EncodedExamplePayload : String
base64EncodedExamplePayload =
    examplePayload
        |> encodeExamplePayload
        |> Json.Encode.encode 0
        |> Bytes.fromUTF8
        |> Base64.encode
        |> Result.withDefault ""


exampleToken : JsonWebToken.Token
exampleToken =
    JsonWebToken.encode JsonWebToken.hmacSha256 encodeExamplePayload exampleSecret examplePayload


exampleTokenWithoutTypHeader : String
exampleTokenWithoutTypHeader =
    "eyJhbGciOiJIUzI1NiJ9.eyJrZXkiOiJ2YWx1ZXMifQ.Yfwr812oBjIBlGfc9eDyOd33f6qlEs3EWGpcw8KK2a4"
