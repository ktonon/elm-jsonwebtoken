# elm-jsonwebtoken

[![elm-package](https://img.shields.io/badge/elm-1.0.1-blue.svg)](http://package.elm-lang.org/packages/ktonon/elm-jsonwebtoken/latest)
[![CircleCI](https://img.shields.io/circleci/project/github/ktonon/elm-jsonwebtoken.svg)](https://circleci.com/gh/ktonon/elm-jsonwebtoken)

JSON Web Token encoder and decoder.

```elm
import JsonWebToken as JWT exposing (hmacSha256)

JWT.encode hmacSha256 Json.Encode.string "secret" "some payload"
    |> JWT.decode Json.Decode.string "secret"
--> Ok "some payload"

JWT.encode hmacSha256 Json.Encode.int "secret" 123
    |> JWT.decode Json.Decode.int "wrong secret"
--> Err <| InvalidSecret 123
```

Currently supports HMAC digests using `SHA224`, `SHA256`, `SHA384`, and `SHA512`. If any other `alg` is used, then `decode` will fail with `DecodeHeaderFailed String`.
