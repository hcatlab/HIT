module Components.Navbar exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)


view :
    { title : String
    , items : List { title : String, href : String }
    }
    -> Html msg
view props =
    Html.nav []
        [ Html.ul []
            [ Html.li []
                [ Html.strong []
                    [ Html.a [ href "/" ] [ Html.text props.title ]
                    ]
                ]
            ]
        , Html.ul []
            (List.map
                (\item ->
                    Html.li []
                        [ Html.a [ Html.Attributes.href item.href ] [ Html.text item.title ]
                        ]
                )
                props.items
            )
        ]
