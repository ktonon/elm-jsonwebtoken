module JsonWebToken.Base64 exposing (decode, encode, encodeHex)

import Base64
import Regex exposing (Regex)
import String.UTF8 as UTF8
import UrlBase64
import Word.Bytes as Bytes
import Word.Hex as Hex


encode : String -> String
encode =
    urlEncode (Bytes.fromUTF8 >> Base64.encode)


encodeHex : String -> String
encodeHex =
    urlEncode (Hex.toByteList >> Base64.encode)


decode : String -> Result String String
decode =
    includeBase64Padding >> Result.andThen (UrlBase64.decode (Base64.decode >> Result.andThen UTF8.toString))



-- URL


urlEncode : (a -> Result String String) -> a -> String
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
        |> Result.withDefault ""
        |> Regex.replace replaceForUrl replaceChar


replaceForUrl : Regex
replaceForUrl =
    Regex.fromString "[\\+=/]"
        |> Maybe.withDefault Regex.never


includeBase64Padding : String -> Result String String
includeBase64Padding text =
    case modBy 4 (String.length text) of
        0 ->
            Ok text

        2 ->
            Ok (text ++ "==")

        3 ->
            Ok (text ++ "=")

        _ ->
            Err "Illegal base64url string!"
