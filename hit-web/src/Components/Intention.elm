module Components.Intention exposing (rank, view)

import Api.Intentions exposing (Intention)
import Components.Palette exposing (paletteFromColor)
import Html exposing (Html, button, div, input, p, small, strong, text)
import Html.Attributes exposing (checked, class, style, type_)
import Html.Events exposing (onCheck, onClick)
import Time


type alias ViewConfig msg =
    { intention : Intention
    , goalColor : String
    , todayWeekday : Maybe Time.Weekday
    , now : Maybe ( Time.Zone, Time.Posix )
    , completed : Bool
    , numberLabel : String
    , onToggle : msg
    , onDelete : msg
    }


view : ViewConfig msg -> Html msg
view config =
    let
        palette =
            paletteFromColor config.goalColor

        r =
            rank config.intention.interval config.intention.deadlineWeekdays config.todayWeekday

        label =
            dayLabel r

        dateText =
            Maybe.map (\now -> formatFutureDate now r) config.now
                |> Maybe.withDefault ""
    in
    div
        [ class "intention"
        , style "background-color" palette.foreground
        , style "color" palette.primary
        , style "border-radius" "8px"
        , style "padding" "0.75rem 1rem"
        , style "margin-bottom" "0.5rem"
        ]
        [ input
            [ type_ "checkbox"
            , checked config.completed
            , onCheck (\_ -> config.onToggle)
            ]
            []
        , strong [ style "margin-right" "0.35rem" ] [ text config.numberLabel ]
        , strong [] [ text (" " ++ config.intention.name) ]
        , small [ style "color" palette.secondary ]
            [ text (" (" ++ config.intention.interval ++ ", rate " ++ config.intention.rate ++ ")") ]
        , if label /= "" then
            small [ style "color" palette.tertiary ]
                [ text (" — " ++ label ++ combinedDateSuffix dateText) ]

          else
            text ""
        , p [ style "color" palette.secondary, style "margin" "0.25rem 0" ]
            [ text (Maybe.withDefault "" config.intention.description) ]
        , button [ class "secondary", onClick config.onDelete ] [ text "Delete" ]
        ]


combinedDateSuffix : String -> String
combinedDateSuffix dateText =
    if dateText == "" then
        ""

    else
        " (" ++ dateText ++ ")"


weekdayToIndex : Time.Weekday -> Int
weekdayToIndex day =
    case day of
        Time.Mon ->
            0

        Time.Tue ->
            1

        Time.Wed ->
            2

        Time.Thu ->
            3

        Time.Fri ->
            4

        Time.Sat ->
            5

        Time.Sun ->
            6


weekdayNameToIndex : String -> Maybe Int
weekdayNameToIndex name =
    case String.toLower name of
        "monday" ->
            Just 0

        "tuesday" ->
            Just 1

        "wednesday" ->
            Just 2

        "thursday" ->
            Just 3

        "friday" ->
            Just 4

        "saturday" ->
            Just 5

        "sunday" ->
            Just 6

        _ ->
            Nothing


{-| Lower rank = sooner. Daily intentions are always "due today" (rank 0).
Weekly intentions rank by the nearest upcoming matching weekday.
-}
rank : String -> List String -> Maybe Time.Weekday -> Int
rank interval deadlineWeekdays maybeToday =
    case maybeToday of
        Nothing ->
            999

        Just today ->
            if interval == "daily" then
                0

            else
                case deadlineWeekdays |> List.filterMap weekdayNameToIndex of
                    [] ->
                        999

                    indices ->
                        let
                            todayIdx =
                                weekdayToIndex today
                        in
                        indices
                            |> List.map (\i -> modBy 7 (i - todayIdx))
                            |> List.minimum
                            |> Maybe.withDefault 999


dayLabel : Int -> String
dayLabel r =
    case r of
        999 ->
            ""

        0 ->
            "Today"

        1 ->
            "Tomorrow"

        n ->
            "In " ++ String.fromInt n ++ " days"


monthAbbrev : Time.Month -> String
monthAbbrev month =
    case month of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"


weekdayAbbrev : Time.Weekday -> String
weekdayAbbrev day =
    case day of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


{-| Formats the actual calendar date `r` days from now, e.g. "Tue, 2 Sep".
-}
formatFutureDate : ( Time.Zone, Time.Posix ) -> Int -> String
formatFutureDate ( zone, now ) r =
    if r == 999 then
        ""

    else
        let
            targetMillis =
                Time.posixToMillis now + (r * 24 * 60 * 60 * 1000)

            targetPosix =
                Time.millisToPosix targetMillis

            wd =
                Time.toWeekday zone targetPosix

            day =
                Time.toDay zone targetPosix

            month =
                Time.toMonth zone targetPosix
        in
        weekdayAbbrev wd ++ ", " ++ String.fromInt day ++ " " ++ monthAbbrev month