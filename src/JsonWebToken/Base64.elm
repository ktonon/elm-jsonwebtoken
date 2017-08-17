module JsonWebToken.Base64 exposing (decode, encode, encodeHex)

import Base64
import Bytes exposing (Bytes)
import Regex exposing (Regex)
import UrlBase64


encode : String -> String
encode =
    urlEncode
        (Bytes.fromUTF8
            >> Bytes.toList
            >> Base64.encode
        )


encodeHex : String -> String
encodeHex =
    urlEncode
        (Bytes.fromHex
            >> Bytes.toList
            >> Base64.encode
        )


decode : String -> Result String String
decode =
    UrlBase64.decode
        (Base64.decode >> Result.andThen listToString)


listToString : List Int -> Result String String
listToString =
    Bytes.fromList >> Result.map Bytes.toString



-- URL


urlEncode : (a -> String) -> a -> String
urlEncode enc t =
    let
        replaceChar { match } =
            case match of
                "+" ->
                    "-"

                "/" ->
                    "_"

                _ ->
                    ""
    in
    enc t
        |> Regex.replace Regex.All replaceForUrl replaceChar


replaceForUrl : Regex
replaceForUrl =
    Regex.regex "[\\+/=]"
