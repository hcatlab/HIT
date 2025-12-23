module Api.Health exposing (..)

import Api exposing (apiUrl)
import Http
import Json.Decode as Decode exposing (Decoder)


getHealth : { onResponse : Result Http.Error Health -> msg } -> Cmd msg
getHealth { onResponse } =
    Http.get
        { url = apiUrl "/health"
        , expect = Http.expectJson onResponse healthDecoder
        }


type alias Health =
    { status : String
    }


healthDecoder : Decoder Health
healthDecoder =
    Decode.map Health (Decode.field "status" Decode.string)
