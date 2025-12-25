module Components.Goal exposing (Model, Msg(..), init, update, view)

import Api.Goals
import Color
import Html exposing (Html, button, div, input, p, small, span, strong, text, textarea)
import Html.Attributes exposing (attribute, class, disabled, placeholder, style, type_, value)
import Html.Events exposing (onBlur, onClick, onInput)
import String exposing (isEmpty)


type alias Model =
    { goal : Api.Goals.Goal
    , palette : Palette
    , isEditing : Bool
    , draftName : String
    , draftDescription : String
    , confirmDelete : Bool
    }


init : Api.Goals.Goal -> Model
init goal =
    { goal = goal
    , palette = paletteFromColor goal.color
    , isEditing = False
    , draftName = goal.name
    , draftDescription = goal.description
    , confirmDelete = False
    }


type Msg
    = ChangeColor String
    | SaveColor
    | ToggleEdit
    | SetName String
    | SetDescription String
    | SaveEdits
    | RequestDelete
    | DeleteGoal


update : Msg -> Model -> ( Model, Maybe Api.Goals.Goal )
update msg model =
    case msg of
        ChangeColor newColor ->
            ( { model | palette = paletteFromColor newColor }, Nothing )

        SaveColor ->
            let
                g =
                    model.goal

                updatedGoal =
                    { g | color = model.palette.background }
            in
            ( { model | goal = updatedGoal }, Just updatedGoal )

        ToggleEdit ->
            ( { model | isEditing = True, draftName = model.goal.name, draftDescription = model.goal.description, confirmDelete = False }
            , Nothing
            )

        SetName s ->
            ( { model | draftName = s }, Nothing )

        SetDescription s ->
            ( { model | draftDescription = s }, Nothing )

        SaveEdits ->
            let
                trimmedName =
                    String.trim model.draftName

                trimmedDesc =
                    String.trim model.draftDescription

                g =
                    model.goal

                updatedGoal =
                    { g | name = trimmedName, description = trimmedDesc }
            in
            ( { model | goal = updatedGoal, isEditing = False, confirmDelete = False }, Just updatedGoal )

        RequestDelete ->
            ( { model | confirmDelete = True }, Nothing )

        DeleteGoal ->
            ( { model | confirmDelete = False }, Just model.goal )


view : Model -> Html Msg
view model =
    let
        goalLabel =
            if model.goal.number > 0 then
                "#" ++ String.fromInt model.goal.number ++ " "

            else
                ""
    in
    div [ class "goal", style "background-color" model.palette.background ]
        [ div [ class "goal-header", style "color" model.palette.primary ]
            (if model.isEditing then
                [ strong []
                    [ text goalLabel
                    ]
                , input
                    [ value model.draftName
                    , placeholder "New goal"
                    , onInput SetName
                    , style "background-color" model.palette.foreground
                    , style "color" model.palette.primary
                    , class "goal-name"
                    ]
                    []
                ]

             else
                [ strong [] [ text (goalLabel ++ model.goal.name) ] ]
            )
        , if model.isEditing then
            textarea
                [ value model.draftDescription
                , placeholder "My goal is..."
                , onInput SetDescription
                , style "background-color" model.palette.foreground
                , style "color" model.palette.secondary
                , class "goal-main"
                ]
                []

          else
            p
                [ class "goal-main"
                , style "color" model.palette.secondary
                ]
                [ text model.goal.description ]
        , div
            [ class "goal-footer"
            , style "color" model.palette.tertiary
            ]
            (if model.goal.startDate /= "" || model.goal.endDate /= Nothing then
                [ small []
                    [ text ("Start Date: " ++ model.goal.startDate)
                    , text " | "
                    , case model.goal.endDate of
                        Just endDate ->
                            text ("End Date: " ++ endDate)

                        Nothing ->
                            text "Active"
                    ]
                ]

             else
                []
            )
        , div [ class "goal-actions" ]
            [ input
                [ type_ "color"
                , value model.palette.background
                , onInput ChangeColor
                , onBlur SaveColor
                ]
                []
            , button
                ([ class "secondary"
                 , onClick
                    (if model.isEditing then
                        SaveEdits

                     else
                        ToggleEdit
                    )
                 ]
                    ++ (if model.isEditing && isEmpty model.draftName && isEmpty model.draftDescription then
                            [ disabled True ]

                        else
                            []
                       )
                )
                [ text
                    (if model.isEditing then
                        "Save"

                     else
                        "Edit"
                    )
                ]
            , button
                [ class "secondary"
                , onClick
                    (if model.confirmDelete then
                        DeleteGoal

                     else
                        RequestDelete
                    )
                ]
                [ text
                    (if model.confirmDelete then
                        "Sure?"

                     else
                        "Delete"
                    )
                ]
            ]
        ]


{-| Palette generated from a background color.
-}
type alias Palette =
    { primary : String
    , secondary : String
    , tertiary : String
    , background : String
    , foreground : String
    }


paletteFromColor : String -> Palette
paletteFromColor backgroundColor =
    case Color.fromHex backgroundColor of
        Just color ->
            let
                primaryColor =
                    if Color.isLight color then
                        Color.black

                    else
                        Color.white

                secondaryColor =
                    if Color.isLight color then
                        Color.mapLightness ((*) 0.25) primaryColor

                    else
                        Color.mapLightness ((*) 0.75) primaryColor

                tertiaryColor =
                    if Color.isLight color then
                        Color.mapLightness ((*) 0.35) primaryColor

                    else
                        Color.mapLightness ((*) 0.65) primaryColor

                foregroundColor =
                    if Color.isLight color then
                        Color.mapSaturation ((*) 0.25) color

                    else
                        Color.mapSaturation ((*) 0.75) color
            in
            { primary = primaryColor |> Color.toCssString
            , secondary = secondaryColor |> Color.toCssString
            , tertiary = tertiaryColor |> Color.toCssString
            , background = backgroundColor
            , foreground = foregroundColor |> Color.toCssString
            }

        Nothing ->
            { primary = "#000000"
            , secondary = "#333333"
            , tertiary = "#777777"
            , background = "#ffffff"
            , foreground = "#aaaaaa"
            }
