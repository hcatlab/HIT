module Api exposing (Data(..), apiUrl, errorToString)

import Http exposing (Error(..))
import Json.Decode as Json


type Data value
    = Loading
    | Success value
    | Failure Http.Error


errorToString : Http.Error -> String
errorToString httpError =
    case httpError of
        Timeout ->
            "Timeout exceeded"

        NetworkError ->
            "Network error"

        BadStatus code ->
            "Failure: HTTP " ++ String.fromInt code

        BadBody text ->
            "Failure: " ++ text

        BadUrl url ->
            "Malformed url: " ++ url


baseUrl : String
baseUrl =
    "http://localhost:8080"


apiUrl : String -> String
apiUrl endpoint =
    if String.startsWith "/" endpoint then
        baseUrl ++ endpoint

    else
        baseUrl ++ "/" ++ endpoint
