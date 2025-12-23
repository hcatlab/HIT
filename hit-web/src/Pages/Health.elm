module Pages.Health exposing (Model, Msg, page)

import Api
import Api.Health exposing (Health)
import Effect exposing (Effect)
import Html exposing (button, div, p, text)
import Html.Attributes exposing (disabled)
import Html.Events exposing (onClick)
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { health : Api.Data Health }


init : () -> ( Model, Effect Msg )
init _ =
    ( { health = Api.Loading }
    , Effect.sendCmd <| Api.Health.getHealth { onResponse = HealthApiResponse }
    )



-- UPDATE


type Msg
    = HealthApiResponse (Result Http.Error Health)
    | RefreshHealth


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        HealthApiResponse (Ok health) ->
            ( { model | health = Api.Success health }, Effect.none )

        HealthApiResponse (Err httpError) ->
            ( { model | health = Api.Failure httpError }, Effect.none )

        RefreshHealth ->
            ( { model | health = Api.Loading }
            , Effect.sendCmd <| Api.Health.getHealth { onResponse = HealthApiResponse }
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Health"
    , body =
        [ div []
            [ case model.health of
                Api.Loading ->
                    p [] [ text "Loading..." ]

                Api.Failure error ->
                    p [] [ text ("Error: " ++ Api.errorToString error) ]

                Api.Success health ->
                    p [] [ text ("Server Status: " ++ health.status) ]
            , button
                [ onClick RefreshHealth
                , disabled (model.health == Api.Loading)
                ]
                [ text "Refresh" ]
            ]
        ]
    }
