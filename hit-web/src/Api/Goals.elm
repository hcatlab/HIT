module Api.Goals exposing
    ( Goal
    , createGoal
    , deleteGoal
    , listGoals
    , placeholderGoal
    , updateGoal
    )

import Api
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


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


createGoal :
    { token : String
    , name : String
    , description : String
    , color : String
    , startDate : Maybe String
    , endDate : Maybe String
    , onResponse : Result Http.Error Goal -> msg
    }
    -> Cmd msg
createGoal { token, name, description, color, startDate, endDate, onResponse } =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl "/goals"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "name", Encode.string name )
                    , ( "description", Encode.string description )
                    , ( "color", Encode.string color )
                    , ( "startDate"
                      , case startDate of
                            Just d ->
                                Encode.string d

                            Nothing ->
                                Encode.null
                      )
                    , ( "endDate"
                      , case endDate of
                            Just d ->
                                Encode.string d

                            Nothing ->
                                Encode.null
                      )
                    ]
        , expect = Http.expectJson onResponse goalDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


updateGoal :
    { token : String
    , id : String
    , name : String
    , description : String
    , color : String
    , startDate : String
    , endDate : Maybe String
    , onResponse : Result Http.Error Goal -> msg
    }
    -> Cmd msg
updateGoal { token, id, name, description, color, startDate, endDate, onResponse } =
    Http.request
        { method = "PUT"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl ("/goals/" ++ id)
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "name", Encode.string name )
                    , ( "description", Encode.string description )
                    , ( "color", Encode.string color )
                    , ( "startDate", Encode.string startDate )
                    , ( "endDate"
                      , case endDate of
                            Just d ->
                                Encode.string d

                            Nothing ->
                                Encode.null
                      )
                    ]
        , expect = Http.expectJson onResponse goalDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteGoal :
    { token : String
    , id : String
    , onResponse : Result Http.Error () -> msg
    }
    -> Cmd msg
deleteGoal { token, id, onResponse } =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl ("/goals/" ++ id)
        , body = Http.emptyBody
        , expect = Http.expectWhatever onResponse
        , timeout = Nothing
        , tracker = Nothing
        }


placeholderGoal : Int -> String -> Goal
placeholderGoal number color =
    { id = ""
    , number = 0
    , name = ""
    , description = ""
    , color = color
    , startDate = ""
    , endDate = Nothing
    , createdAt = ""
    , modifiedAt = ""
    }
