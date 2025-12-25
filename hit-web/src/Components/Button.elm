module Components.Button exposing (..)

import Color
import Html exposing (Attribute, Html)
import Html.Attributes as Attr


accentFromBgColor : String -> String
accentFromBgColor backgroundColor =
    case Color.fromHex backgroundColor of
        Just color ->
            if Color.isLight color then
                Color.black |> Color.toCssString

            else
                Color.white |> Color.toCssString

        Nothing ->
            "#000000"


view : { backgroundColor : String } -> List (Attribute msg) -> List (Html msg) -> Html msg
view props attributes children =
    Html.button
        ([ Attr.style "background-color" props.backgroundColor
         , Attr.style "color" (accentFromBgColor props.backgroundColor)
         , Attr.class "button"
         ]
            ++ attributes
        )
        children
