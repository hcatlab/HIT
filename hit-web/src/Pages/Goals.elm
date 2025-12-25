module Pages.Goals exposing (Model, Msg, page)

import Api
import Api.Goals exposing (Goal, placeholderGoal)
import Color
import Components.Button as Button
import Components.Goal as Goal exposing (Msg(..))
import Components.Listing as Listing
import Dict
import Effect exposing (Effect)
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Http
import Layouts
import Page exposing (Page)
import Platform
import Random
import Route exposing (Route)
import Route.Path
import Shared
import Task
import Time
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
    , placeholderColor : String
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    case shared.token of
        Just token ->
            ( { goals = Api.Loading
              , token = Just token
              , placeholderColor = ""
              }
            , Effect.batch
                [ Effect.sendCmd <|
                    Api.Goals.listGoals
                        { token = token
                        , onResponse = GoalsResponse
                        }
                , Effect.sendCmd <|
                    Random.generate GotColor colorGen
                ]
            )

        Nothing ->
            ( { goals = Api.Loading
              , token = Nothing
              , placeholderColor = ""
              }
            , Effect.sendCmd <|
                Random.generate GotColor colorGen
            )



-- UPDATE


type Msg
    = GoalsResponse (Result Http.Error (List Api.Goals.Goal))
    | GoalMsg Int Goal.Msg
    | GoalUpdated Int (Result Http.Error Api.Goals.Goal)
    | GoalCreated Int (Result Http.Error Api.Goals.Goal)
    | GotColor String
    | AddNewGoal
    | GoalDeleted String (Result Http.Error ())


colorGen : Random.Generator String
colorGen =
    Random.map Color.toCssString
        (Random.map3 Color.rgb255
            (Random.int 0 255)
            (Random.int 0 255)
            (Random.int 0 255)
        )


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GotColor color ->
            ( { model | placeholderColor = color }, Effect.none )

        GoalsResponse (Ok goals) ->
            let
                baseModels =
                    List.map Goal.init goals
            in
            ( { model | goals = Api.Success baseModels }
            , Effect.none
            )

        AddNewGoal ->
            case model.goals of
                Api.Success goals ->
                    let
                        newModel =
                            let
                                gm =
                                    Goal.init (placeholderGoal (List.length goals + 1) model.placeholderColor)
                            in
                            { gm | isEditing = True, draftName = gm.draftName, draftDescription = gm.draftDescription }

                        models =
                            goals ++ [ newModel ]
                    in
                    ( { model | goals = Api.Success models }, Effect.none )

                _ ->
                    ( model, Effect.none )

        GoalsResponse (Err error) ->
            ( { model | goals = Api.Failure error }
            , Effect.none
            )

        GoalMsg idx subMsg ->
            case model.goals of
                Api.Success goalModels ->
                    let
                        goalAtIndex =
                            List.drop idx goalModels
                                |> List.head
                    in
                    case subMsg of
                        DeleteGoal ->
                            case goalAtIndex of
                                Just goal ->
                                    if goal.goal.id == "" then
                                        let
                                            models =
                                                List.take idx goalModels
                                                    ++ List.drop (idx + 1) goalModels
                                        in
                                        ( { model | goals = Api.Success models }, Effect.none )

                                    else
                                        let
                                            effect =
                                                case model.token of
                                                    Just token ->
                                                        Effect.sendCmd <|
                                                            Api.Goals.deleteGoal
                                                                { token = token
                                                                , id = goal.goal.id
                                                                , onResponse = GoalDeleted goal.goal.id
                                                                }

                                                    Nothing ->
                                                        Effect.none
                                        in
                                        ( model, effect )

                                Nothing ->
                                    ( model, Effect.none )

                        _ ->
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
                                            let
                                                updateCmd =
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

                                                createCmd =
                                                    let
                                                        startDateMaybe =
                                                            if goal.startDate == "" then
                                                                Nothing

                                                            else
                                                                Just goal.startDate
                                                    in
                                                    Api.Goals.createGoal
                                                        { token = token
                                                        , name = goal.name
                                                        , description = goal.description
                                                        , color = goal.color
                                                        , startDate = startDateMaybe
                                                        , endDate = goal.endDate
                                                        , onResponse = GoalCreated idx
                                                        }
                                            in
                                            case subMsg of
                                                Goal.SaveEdits ->
                                                    if goal.id == "" then
                                                        Effect.sendCmd <| createCmd

                                                    else
                                                        Effect.sendCmd <| updateCmd

                                                _ ->
                                                    if goal.id == "" then
                                                        Effect.none

                                                    else
                                                        Effect.sendCmd <| updateCmd

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

        GoalDeleted goalId (Ok _) ->
            case model.goals of
                Api.Success goalModels ->
                    let
                        filtered =
                            List.filter (\rm -> rm.goal.id /= goalId) goalModels
                    in
                    ( { model | goals = Api.Success filtered }, Effect.none )

                _ ->
                    ( model, Effect.none )

        GoalDeleted _ (Err _) ->
            ( model, Effect.none )

        GoalCreated idx (Ok goal) ->
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

        GoalCreated _ (Err _) ->
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
                    let
                        indexed =
                            List.indexedMap Tuple.pair goals

                        addButton =
                            Button.view
                                { backgroundColor = model.placeholderColor }
                                [ Attr.class "add-goal-button"
                                , onClick AddNewGoal
                                ]
                                [ text "+ Add Goal" ]
                    in
                    div []
                        [ Listing.view
                            { items = indexed
                            , render = \( i, gm ) -> Html.map (GoalMsg i) (Goal.view gm)
                            , listClass = "goal-list"
                            , itemClass = "goal-item"
                            }
                        , addButton
                        ]
            ]
        ]
    }
