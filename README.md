# elm-jsonwebtoken

[![CircleCI](https://img.shields.io/circleci/project/github/ktonon/elm-jsonwebtoken.svg)](https://circleci.com/gh/ktonon/elm-jsonwebtoken)

JSON Web Token encoder and decoder.

```elm
import JsonWebToken as JWT exposing (hmacSha256, DecodeError(UnexpectedError))

JWT.encode hmacSha256 Json.Encode.string "secret" "some payload"
    |> Result.mapError UnexpectedError
    |> Result.andThen (JWT.decode Json.Decode.string "secret")
--> Ok "some payload"

JWT.encode hmacSha256 Json.Encode.int "secret" 123
    |> Result.mapError UnexpectedError
    |> Result.andThen (JWT.decode Json.Decode.int "wrong secret")
--> Err <| InvalidSecret 123
```

Currently only supports HMAC digests using `SHA1` or `SHA256`. If any other `alg` is used, then `decode` will fail with `DecodeHeaderFailed String`.
