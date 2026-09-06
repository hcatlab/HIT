module Pages.Intentions exposing (Model, Msg, page)

import Api
import Api.Goals
import Api.Intentions exposing (Intention)
import Components.Intention as Intention
import Effect exposing (Effect)
import Html exposing (Html, button, div, h2, h3, input, label, li, p, text, textarea, ul)
import Html.Attributes exposing (checked, class, disabled, placeholder, style, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Set exposing (Set)
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
    { intentions : Api.Data (List Intention)
    , goals : Api.Data (List Api.Goals.Goal)
    , token : Maybe String
    , todayWeekday : Maybe Time.Weekday
    , now : Maybe ( Time.Zone, Time.Posix )
    , completedToday : Set String
    , draftName : String
    , draftDescription : String
    , draftGoalIds : List String
    , draftDays : Set String
    }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    let
        baseModel =
            { intentions = Api.Loading
            , goals = Api.Loading
            , token = shared.token
            , todayWeekday = Nothing
            , now = Nothing
            , completedToday = Set.empty
            , draftName = ""
            , draftDescription = ""
            , draftGoalIds = []
            , draftDays = Set.empty
            }
    in
    case shared.token of
        Just token ->
            ( baseModel
            , Effect.batch
                [ Effect.sendCmd <|
                    Api.Intentions.listIntentions
                        { token = token, interval = Nothing, onResponse = IntentionsResponse }
                , Effect.sendCmd <|
                    Api.Goals.listGoals { token = token, onResponse = GoalsResponse }
                , Effect.sendCmd <|
                    Api.Intentions.listTodayCompletions
                        { token = token, onResponse = TodayCompletionsResponse }
                , Effect.sendCmd <|
                    Task.perform GotTime (Task.map2 Tuple.pair Time.here Time.now)
                ]
            )

        Nothing ->
            ( baseModel, Effect.none )



-- UPDATE


type Msg
    = IntentionsResponse (Result Http.Error (List Intention))
    | GoalsResponse (Result Http.Error (List Api.Goals.Goal))
    | TodayCompletionsResponse (Result Http.Error (List String))
    | GotTime ( Time.Zone, Time.Posix )
    | ToggleCompleted String
    | CompletionSaved (Result Http.Error ())
    | RequestDelete String String
    | IntentionDeleted String (Result Http.Error ())
    | SetDraftName String
    | SetDraftDescription String
    | ToggleDraftGoal String Bool
    | ToggleDraftDay String Bool
    | SubmitCreate
    | IntentionCreated (Result Http.Error Intention)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        IntentionsResponse (Ok intentions) ->
            ( { model | intentions = Api.Success intentions }, Effect.none )

        IntentionsResponse (Err error) ->
            ( { model | intentions = Api.Failure error }, Effect.none )

        GoalsResponse (Ok goals) ->
            ( { model | goals = Api.Success goals }, Effect.none )

        GoalsResponse (Err error) ->
            ( { model | goals = Api.Failure error }, Effect.none )

        TodayCompletionsResponse (Ok ids) ->
            ( { model | completedToday = Set.fromList ids }, Effect.none )

        TodayCompletionsResponse (Err _) ->
            ( model, Effect.none )

        GotTime ( zone, time ) ->
            ( { model | todayWeekday = Just (Time.toWeekday zone time), now = Just ( zone, time ) }
            , Effect.none
            )

        ToggleCompleted intentionId ->
            let
                isCurrentlyCompleted =
                    Set.member intentionId model.completedToday

                newSet =
                    if isCurrentlyCompleted then
                        Set.remove intentionId model.completedToday

                    else
                        Set.insert intentionId model.completedToday

                effect =
                    case model.token of
                        Just token ->
                            if isCurrentlyCompleted then
                                Effect.sendCmd <|
                                    Api.Intentions.unmarkComplete
                                        { token = token, id = intentionId, onResponse = CompletionSaved }

                            else
                                Effect.sendCmd <|
                                    Api.Intentions.markComplete
                                        { token = token, id = intentionId, onResponse = CompletionSaved }

                        Nothing ->
                            Effect.none
            in
            ( { model | completedToday = newSet }, effect )

        CompletionSaved (Ok _) ->
            ( model, Effect.none )

        CompletionSaved (Err _) ->
            case model.token of
                Just token ->
                    ( model
                    , Effect.sendCmd <|
                        Api.Intentions.listTodayCompletions
                            { token = token, onResponse = TodayCompletionsResponse }
                    )

                Nothing ->
                    ( model, Effect.none )

        RequestDelete interval intentionId ->
            case model.token of
                Just token ->
                    ( model
                    , Effect.sendCmd <|
                        Api.Intentions.deleteIntention
                            { token = token
                            , interval = interval
                            , id = intentionId
                            , onResponse = IntentionDeleted intentionId
                            }
                    )

                Nothing ->
                    ( model, Effect.none )

        IntentionDeleted intentionId (Ok _) ->
            case model.intentions of
                Api.Success intentions ->
                    ( { model | intentions = Api.Success (List.filter (\i -> i.id /= intentionId) intentions) }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        IntentionDeleted _ (Err _) ->
            ( model, Effect.none )

        SetDraftName s ->
            ( { model | draftName = s }, Effect.none )

        SetDraftDescription s ->
            ( { model | draftDescription = s }, Effect.none )

        ToggleDraftGoal goalId isChecked ->
            let
                newGoalIds =
                    if isChecked then
                        goalId :: model.draftGoalIds

                    else
                        List.filter ((/=) goalId) model.draftGoalIds
            in
            ( { model | draftGoalIds = newGoalIds }, Effect.none )

        ToggleDraftDay day isChecked ->
            let
                newDays =
                    if isChecked then
                        Set.insert day model.draftDays

                    else
                        Set.remove day model.draftDays
            in
            ( { model | draftDays = newDays }, Effect.none )

        SubmitCreate ->
            case model.token of
                Just token ->
                    if Set.isEmpty model.draftDays then
                        ( model
                        , Effect.sendCmd <|
                            Api.Intentions.createDailyIntention
                                { token = token
                                , name = model.draftName
                                , description = model.draftDescription
                                , numerator = 1
                                , denominator = 1
                                , deadlineHours = []
                                , goalIds = model.draftGoalIds
                                , onResponse = IntentionCreated
                                }
                        )

                    else
                        ( model
                        , Effect.sendCmd <|
                            Api.Intentions.createWeeklyIntention
                                { token = token
                                , name = model.draftName
                                , description = model.draftDescription
                                , numerator = Set.size model.draftDays
                                , denominator = 7
                                , selectedDays = Set.toList model.draftDays
                                , goalIds = model.draftGoalIds
                                , onResponse = IntentionCreated
                                }
                        )

                Nothing ->
                    ( model, Effect.none )

        IntentionCreated (Ok _) ->
            let
                clearedModel =
                    { model
                        | draftName = ""
                        , draftDescription = ""
                        , draftGoalIds = []
                        , draftDays = Set.empty
                    }
            in
            case model.token of
                Just token ->
                    ( clearedModel
                    , Effect.sendCmd <|
                        Api.Intentions.listIntentions
                            { token = token, interval = Nothing, onResponse = IntentionsResponse }
                    )

                Nothing ->
                    ( clearedModel, Effect.none )

        IntentionCreated (Err _) ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "My Intentions"
    , body =
        [ div []
            [ h2 [] [ text "My Intentions" ]
            , case ( model.intentions, model.goals ) of
                ( Api.Loading, _ ) ->
                    text "Loading..."

                ( _, Api.Loading ) ->
                    text "Loading..."

                ( Api.Failure error, _ ) ->
                    p [] [ text ("Error loading intentions: " ++ Api.errorToString error) ]

                ( _, Api.Failure error ) ->
                    p [] [ text ("Error loading goals: " ++ Api.errorToString error) ]

                ( Api.Success intentions, Api.Success goals ) ->
                    viewGrouped model intentions goals
            , h3 [] [ text "New Intention" ]
            , viewForm model
            ]
        ]
    }


viewGrouped : Model -> List Intention -> List Api.Goals.Goal -> Html Msg
viewGrouped model intentions goals =
    let
        sortedGoals =
            List.sortBy .number goals
    in
    div [ class "intention-sections" ]
        (List.map (viewGoalSection model intentions) sortedGoals)


viewGoalSection : Model -> List Intention -> Api.Goals.Goal -> Html Msg
viewGoalSection model intentions goal =
    let
        goalIntentions =
            intentions
                |> List.filter (\i -> List.member goal.id i.goalIds)
                |> List.sortBy (\i -> Intention.rank i.interval i.deadlineWeekdays model.todayWeekday)
    in
    div
        [ class "goal-intention-section"
        , style "border-left" ("6px solid " ++ goal.color)
        , style "padding-left" "1rem"
        , style "margin-bottom" "1.5rem"
        ]
        [ h3 [ style "color" goal.color ]
            [ text ("#" ++ String.fromInt goal.number ++ " " ++ goal.name) ]
        , if List.isEmpty goalIntentions then
            p [] [ text "No intentions linked to this goal yet." ]

          else
            ul [ class "intention-list", style "list-style" "none", style "padding" "0" ]
                (List.indexedMap
                    (\idx i ->
                        li []
                            [ Intention.view
                                { intention = i
                                , goalColor = goal.color
                                , todayWeekday = model.todayWeekday
                                , now = model.now
                                , completed = Set.member i.id model.completedToday
                                , numberLabel = String.fromInt goal.number ++ "." ++ String.fromInt (idx + 1)
                                , onToggle = ToggleCompleted i.id
                                , onDelete = RequestDelete i.interval i.id
                                }
                            ]
                    )
                    goalIntentions
                )
        ]


dayOptions : List ( String, String )
dayOptions =
    [ ( "monday", "Mon" )
    , ( "tuesday", "Tue" )
    , ( "wednesday", "Wed" )
    , ( "thursday", "Thu" )
    , ( "friday", "Fri" )
    , ( "saturday", "Sat" )
    , ( "sunday", "Sun" )
    ]


viewForm : Model -> Html Msg
viewForm model =
    let
        goalCheckboxes =
            case model.goals of
                Api.Success goals ->
                    ul []
                        (List.map
                            (\g ->
                                li []
                                    [ label []
                                        [ input
                                            [ type_ "checkbox"
                                            , checked (List.member g.id model.draftGoalIds)
                                            , onCheck (ToggleDraftGoal g.id)
                                            ]
                                            []
                                        , text (" " ++ g.name)
                                        ]
                                    ]
                            )
                            goals
                        )

                Api.Loading ->
                    text "Loading goals..."

                Api.Failure _ ->
                    text "Could not load goals."

        dayCheckboxes =
            div [ style "display" "flex", style "gap" "0.75rem", style "flex-wrap" "wrap" ]
                (List.map
                    (\( value_, label_ ) ->
                        label [ style "display" "flex", style "align-items" "center", style "gap" "0.25rem" ]
                            [ input
                                [ type_ "checkbox"
                                , checked (Set.member value_ model.draftDays)
                                , onCheck (ToggleDraftDay value_)
                                ]
                                []
                            , text label_
                            ]
                    )
                    dayOptions
                )

        canSubmit =
            not (String.isEmpty (String.trim model.draftName))
                && not (List.isEmpty model.draftGoalIds)
    in
    div [ class "intention-form" ]
        [ div []
            [ label [] [ text "Name" ]
            , input [ value model.draftName, placeholder "Morning walk", onInput SetDraftName ] []
            ]
        , div []
            [ label [] [ text "Description" ]
            , textarea [ value model.draftDescription, placeholder "Walk for 20 minutes", onInput SetDraftDescription ] []
            ]
        , div []
            [ label [] [ text "Which days? (leave all unchecked for every day)" ]
            , dayCheckboxes
            ]
        , div []
            [ label [] [ text "Linked goals (at least one required)" ]
            , goalCheckboxes
            ]
        , button
            [ onClick SubmitCreate
            , disabled (not canSubmit)
            ]
            [ text "Create Intention" ]
        ]