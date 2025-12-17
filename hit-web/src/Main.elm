module Main exposing (main)

import Browser
import Html exposing (Html, article, button, h2, p, span, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { serverStatus : ServerStatus
    }


type ServerStatus
    = Unknown
    | Loading
    | Ok String
    | Error String


init : () -> ( Model, Cmd Msg )
init () =
    ( { serverStatus = Unknown }, Cmd.none )


type Msg
    = CheckHealth
    | HealthResponse (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckHealth ->
            ( { model | serverStatus = Loading }
            , Http.get
                { url = "http://localhost:8080/health"
                , expect = Http.expectJson HealthResponse (Decode.field "status" Decode.string)
                }
            )

        HealthResponse result ->
            case result of
                Result.Ok status ->
                    if status == "ok" then
                        ( { model | serverStatus = Ok status }, Cmd.none )

                    else
                        ( { model | serverStatus = Error ("Unexpected status: " ++ status) }, Cmd.none )

                Result.Err err ->
                    ( { model | serverStatus = Error (httpErrorToString err) }, Cmd.none )


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Request timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "Bad status: " ++ String.fromInt code

        Http.BadBody body ->
            "Bad response body: " ++ body


view : Model -> Html Msg
view model =
    article []
        [ h2 [] [ text "Server Status" ]
        , p [] [ text "Check the connection to the HIT backend server." ]
        , p []
            [ button [ onClick CheckHealth ] [ text "Check Server Health" ]
            , text "  "
            , statusIndicator model.serverStatus
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- UI helpers


statusIndicator : ServerStatus -> Html Msg
statusIndicator serverStatus =
    let
        ( color, label ) =
            case serverStatus of
                Unknown ->
                    ( "var(--pico-muted-color, gray)", "• Unknown" )

                Loading ->
                    ( "var(--pico-warning, orange)", "… Checking" )

                Ok status ->
                    ( "var(--pico-primary, green)", "✓ " ++ status )

                Error err ->
                    ( "var(--pico-del-color, red)", "✗ " ++ err )
    in
    span [ style "color" color ] [ text label ]
