module JsonWebTokenTests exposing (all)

import Expect
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
        ]
