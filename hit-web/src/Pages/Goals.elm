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
    { goals : Api.Data (List Goal.Model)
    , token : Maybe String
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    case shared.token of
        Just token ->
            ( { goals = Api.Loading, token = Just token }
            , Effect.sendCmd <|
                Api.Goals.listGoals
                    { token = token
                    , onResponse = GoalsResponse
                    }
            )

        Nothing ->
            ( { goals = Api.Loading, token = Nothing }
            , Effect.pushRoute { path = Route.Path.Login, query = Dict.empty, hash = Nothing }
            )



-- UPDATE


type Msg
    = GoalsResponse (Result Http.Error (List Api.Goals.Goal))
    | GoalMsg Int Goal.Msg
    | GoalUpdated Int (Result Http.Error Api.Goals.Goal)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GoalsResponse (Ok goals) ->
            ( { model | goals = Api.Success (List.map Goal.init goals) }
            , Effect.none
            )

        GoalsResponse (Err error) ->
            ( { model | goals = Api.Failure error }
            , Effect.none
            )

        GoalMsg idx subMsg ->
            case model.goals of
                Api.Success goalModels ->
                    let
                        updatedModels =
                            List.indexedMap
                                (\i gm ->
                                    if i == idx then
                                        Tuple.first (Goal.update subMsg gm)

                                    else
                                        gm
                                )
                                goalModels

                        maybeGoal =
                            List.drop idx updatedModels
                                |> List.head
                                |> Maybe.andThen (\gm -> Tuple.second (Goal.update subMsg gm))

                        effect =
                            case ( maybeGoal, model.token ) of
                                ( Just goal, Just token ) ->
                                    Effect.sendCmd <|
                                        Api.Goals.updateGoal
                                            { token = token
                                            , id = goal.id
                                            , name = goal.name
                                            , description = goal.description
                                            , color = goal.color
                                            , startDate = goal.startDate
                                            , endDate = goal.endDate
                                            , onResponse = GoalUpdated idx
                                            }

                                _ ->
                                    Effect.none
                    in
                    ( { model | goals = Api.Success updatedModels }, effect )

                _ ->
                    ( model, Effect.none )

        GoalUpdated idx (Ok goal) ->
            case model.goals of
                Api.Success goalModels ->
                    let
                        updated =
                            List.indexedMap
                                (\i gm ->
                                    if i == idx then
                                        Goal.init goal

                                    else
                                        gm
                                )
                                goalModels
                    in
                    ( { model | goals = Api.Success updated }, Effect.none )

                _ ->
                    ( model, Effect.none )

        GoalUpdated _ (Err _) ->
            ( model, Effect.none )



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
                        let
                            indexed =
                                List.indexedMap Tuple.pair goals
                        in
                        Listing.view
                            { items = indexed
                            , render = \( i, gm ) -> Html.map (GoalMsg i) (Goal.view gm)
                            , listClass = "goal-list"
                            , itemClass = "goal-item"
                            }
            ]
        ]
    }
