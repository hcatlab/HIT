module Components.Palette exposing (Palette, paletteFromColor)

import Color


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
