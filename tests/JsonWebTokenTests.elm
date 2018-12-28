module JsonWebTokenTests exposing (all)

import Expect
import Json.Decode
import JsonWebToken exposing (DecodeError(..), decode)
import Test exposing (Test, describe, test)
import TestHelpers
    exposing
        ( aValidToken
        , correctSecret
        , payload
        , payloadDecoder
        , wrongSecret
        )


all : Test
all =
    describe "JsonWebToken.verify"
        [ test "verify token with too few parts" <|
            \_ ->
                Expect.equal
                    (Err InvalidToken)
                    (decode payloadDecoder correctSecret "foo.bar")
        , test "verify token with too many parts" <|
            \_ ->
                Expect.equal
                    (Err InvalidToken)
                    (decode payloadDecoder correctSecret "foo.bar.car.far")
        , test "verify token with valid secret" <|
            \_ ->
                Expect.equal
                    (Ok payload)
                    (decode payloadDecoder correctSecret aValidToken)
        , test "decode a token without a typ in the header" <|
            \_ ->
                Expect.equal
                    (Ok "payload")
                    (decode Json.Decode.string "secret" tokenWithoutTyp)
        ]


{-|

  - "alg":"HS256"
  - "payload"
  - signed with "secret"

-}
tokenWithoutTyp : String
tokenWithoutTyp =
    "eyJhbGciOiJIUzI1NiJ9.InBheWxvYWQi.xZ3HN7F1t9dBMbKCXa9pye1VW6wC2A7V93Pva5jpkpI="
