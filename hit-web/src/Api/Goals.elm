module Api.Goals exposing
    ( Goal
    , listGoals
    )

import Api
import Http
import Json.Decode as Decode exposing (Decoder)


type alias Goal =
    { id : String
    , number : Int
    , name : String
    , description : String
    , color : String
    , startDate : String
    , endDate : Maybe String
    , createdAt : String
    , modifiedAt : String
    }


goalDecoder : Decoder Goal
goalDecoder =
    Decode.map8
        (\id_ number_ name_ description_ color_ startDate_ endDate_ createdAt_ ->
            { id = id_
            , number = number_
            , name = name_
            , description = description_
            , color = color_
            , startDate = startDate_
            , endDate = endDate_
            , createdAt = createdAt_
            , modifiedAt = createdAt_
            }
        )
        (Decode.field "id" Decode.string)
        (Decode.field "number" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "description" Decode.string)
        (Decode.field "color" Decode.string)
        (Decode.field "startDate" Decode.string)
        (Decode.field "endDate" (Decode.nullable Decode.string))
        (Decode.field "createdAt" Decode.string)
        |> Decode.andThen
            (\goal ->
                Decode.map
                    (\modifiedAt_ -> { goal | modifiedAt = modifiedAt_ })
                    (Decode.field "modifiedAt" Decode.string)
            )


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
