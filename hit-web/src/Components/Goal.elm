module Components.Goal exposing (view)

import Api.Goals
import Html exposing (Html, div, p, small, strong, text)
import Html.Attributes exposing (class, style)


view : Api.Goals.Goal -> Html msg
view goal =
    div [ class "goal", style "background-color" goal.color ]
        [ div [ class "goal-header" ]
            [ strong [] [ text ("#" ++ String.fromInt goal.number ++ " " ++ goal.name) ]
            ]
        , p [ class "goal-main" ]
            [ text goal.description
            ]
        , div [ class "goal-footer" ]
            [ small []
                [ text ("Start Date: " ++ goal.startDate)
                , text " | "
                , case goal.endDate of
                    Just endDate ->
                        text ("End Date: " ++ endDate)

                    Nothing ->
                        text "Active"
                ]
            ]
        ]
