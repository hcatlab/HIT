module Api.Goals exposing
    ( Goal
    , listGoals
    )

import Api
import Http
import Json.Decode as Decode exposing (Decoder)


type alias Goal =
    { id : String
    , name : String
    , description : Maybe String
    , createdAt : String
    , modifiedAt : String
    }


goalDecoder : Decoder Goal
goalDecoder =
    Decode.map5 Goal
        (Decode.field "id" Decode.string)
        (Decode.field "name" Decode.string)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "createdAt" Decode.string)
        (Decode.field "modifiedAt" Decode.string)


listGoals :
    { token : String, onResponse : Result Http.Error (List Goal) -> msg }
    -> Cmd msg
listGoals { token, onResponse } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl "/goals"
        , body = Http.emptyBody
        , expect = Http.expectJson onResponse (Decode.list goalDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }
