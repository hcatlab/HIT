module Pages.Goals exposing (Model, Msg, page)

import Api
import Api.Goals
import Components.Goal as Goal
import Components.Listing as Listing
import Dict
import Effect exposing (Effect)
import Html exposing (Html, div, text)
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared _ =
    Page.new
        { init = init shared
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { goals : Api.Data (List Api.Goals.Goal)
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    case shared.token of
        Just token ->
            ( { goals = Api.Loading }
            , Effect.sendCmd <|
                Api.Goals.listGoals
                    { token = token
                    , onResponse = GoalsResponse
                    }
            )

        Nothing ->
            ( { goals = Api.Loading }
            , Effect.pushRoute { path = Route.Path.Login, query = Dict.empty, hash = Nothing }
            )



-- UPDATE


type Msg
    = GoalsResponse (Result Http.Error (List Api.Goals.Goal))


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GoalsResponse (Ok goals) ->
            ( { model | goals = Api.Success goals }
            , Effect.none
            )

        GoalsResponse (Err error) ->
            ( { model | goals = Api.Failure error }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "My Goals"
    , body =
        [ div []
            [ Html.h2 [] [ text "My Goals" ]
            , case model.goals of
                Api.Loading ->
                    text "Loading goals..."

                Api.Failure error ->
                    Html.p [] [ text ("Error loading goals: " ++ Api.errorToString error) ]

                Api.Success goals ->
                    if List.isEmpty goals then
                        Html.p [] [ text "No goals yet. Create one to get started!" ]

                    else
                        Html.map never
                            (Listing.view
                                { items = goals
                                , render = Goal.view
                                , listClass = "goal-list"
                                , itemClass = "goal-item"
                                }
                            )
            ]
        ]
    }
