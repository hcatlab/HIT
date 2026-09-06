module Api.Intentions exposing
    ( Intention
    , createDailyIntention
    , createWeeklyIntention
    , deleteIntention
    , listIntentions
    , listTodayCompletions
    , markComplete
    , unmarkComplete
    )

import Api
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias Intention =
    { id : String
    , interval : String
    , name : String
    , description : Maybe String
    , rate : String
    , deadlineHours : List Int
    , deadlineWeekdays : List String
    , goalIds : List String
    , createdAt : String
    , modifiedAt : String
    }


decodeDeadline : Decoder ( List Int, List String )
decodeDeadline =
    Decode.oneOf
        [ Decode.list Decode.int |> Decode.map (\hours -> ( hours, [] ))
        , Decode.keyValuePairs (Decode.list Decode.int)
            |> Decode.map
                (\pairs ->
                    ( []
                    , pairs
                        |> List.filter (\( _, hours ) -> not (List.isEmpty hours))
                        |> List.map Tuple.first
                    )
                )
        , Decode.succeed ( [], [] )
        ]


intentionDecoder : Decoder Intention
intentionDecoder =
    Decode.map8
        (\id_ interval_ name_ description_ rate_ ( hours, weekdays ) goalIds_ createdAt_ ->
            { id = id_
            , interval = interval_
            , name = name_
            , description = description_
            , rate = rate_
            , deadlineHours = hours
            , deadlineWeekdays = weekdays
            , goalIds = goalIds_
            , createdAt = createdAt_
            , modifiedAt = ""
            }
        )
        (Decode.field "id" Decode.string)
        (Decode.oneOf [ Decode.field "interval" Decode.string, Decode.succeed "daily" ])
        (Decode.field "name" Decode.string)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "rate" Decode.string)
        (Decode.field "deadline" decodeDeadline)
        (Decode.field "goalIds" (Decode.list Decode.string))
        (Decode.field "createdAt" Decode.string)
        |> Decode.andThen
            (\partial ->
                Decode.map (\m -> { partial | modifiedAt = m }) (Decode.field "modifiedAt" Decode.string)
            )


listIntentions :
    { token : String, interval : Maybe String, onResponse : Result Http.Error (List Intention) -> msg }
    -> Cmd msg
listIntentions { token, interval, onResponse } =
    let
        query =
            case interval of
                Just i ->
                    "?interval=" ++ i

                Nothing ->
                    ""
    in
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl ("/intentions" ++ query)
        , body = Http.emptyBody
        , expect = Http.expectJson onResponse (Decode.list intentionDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }


createDailyIntention :
    { token : String
    , name : String
    , description : String
    , numerator : Int
    , denominator : Int
    , deadlineHours : List Int
    , goalIds : List String
    , onResponse : Result Http.Error Intention -> msg
    }
    -> Cmd msg
createDailyIntention { token, name, description, numerator, denominator, deadlineHours, goalIds, onResponse } =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl "/intentions/daily"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "name", Encode.string name )
                    , ( "description", Encode.string description )
                    , ( "rate", Encode.string (String.fromInt numerator ++ "/" ++ String.fromInt denominator) )
                    , ( "deadline", Encode.list Encode.int deadlineHours )
                    , ( "goalIds", Encode.list Encode.string goalIds )
                    ]
        , expect = Http.expectJson onResponse intentionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


createWeeklyIntention :
    { token : String
    , name : String
    , description : String
    , numerator : Int
    , denominator : Int
    , selectedDays : List String
    , goalIds : List String
    , onResponse : Result Http.Error Intention -> msg
    }
    -> Cmd msg
createWeeklyIntention { token, name, description, numerator, denominator, selectedDays, goalIds, onResponse } =
    let
        dayValue day =
            if List.member day selectedDays then
                Encode.list Encode.int [ 9 ]

            else
                Encode.list Encode.int []

        deadlineObject =
            Encode.object
                [ ( "monday", dayValue "monday" )
                , ( "tuesday", dayValue "tuesday" )
                , ( "wednesday", dayValue "wednesday" )
                , ( "thursday", dayValue "thursday" )
                , ( "friday", dayValue "friday" )
                , ( "saturday", dayValue "saturday" )
                , ( "sunday", dayValue "sunday" )
                ]
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl "/intentions/weekly"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "name", Encode.string name )
                    , ( "description", Encode.string description )
                    , ( "rate", Encode.string (String.fromInt numerator ++ "/" ++ String.fromInt denominator) )
                    , ( "deadline", deadlineObject )
                    , ( "goalIds", Encode.list Encode.string goalIds )
                    ]
        , expect = Http.expectJson onResponse intentionDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


deleteIntention :
    { token : String
    , interval : String
    , id : String
    , onResponse : Result Http.Error () -> msg
    }
    -> Cmd msg
deleteIntention { token, interval, id, onResponse } =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl ("/intentions/" ++ interval ++ "/" ++ id)
        , body = Http.emptyBody
        , expect = Http.expectWhatever onResponse
        , timeout = Nothing
        , tracker = Nothing
        }


listTodayCompletions :
    { token : String, onResponse : Result Http.Error (List String) -> msg }
    -> Cmd msg
listTodayCompletions { token, onResponse } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl "/intentions/completions/today"
        , body = Http.emptyBody
        , expect = Http.expectJson onResponse (Decode.list Decode.string)
        , timeout = Nothing
        , tracker = Nothing
        }


markComplete :
    { token : String, id : String, onResponse : Result Http.Error () -> msg }
    -> Cmd msg
markComplete { token, id, onResponse } =
    Http.request
        { method = "POST"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl ("/intentions/" ++ id ++ "/complete")
        , body = Http.emptyBody
        , expect = Http.expectWhatever onResponse
        , timeout = Nothing
        , tracker = Nothing
        }


unmarkComplete :
    { token : String, id : String, onResponse : Result Http.Error () -> msg }
    -> Cmd msg
unmarkComplete { token, id, onResponse } =
    Http.request
        { method = "DELETE"
        , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = Api.apiUrl ("/intentions/" ++ id ++ "/complete")
        , body = Http.emptyBody
        , expect = Http.expectWhatever onResponse
        , timeout = Nothing
        , tracker = Nothing
        }