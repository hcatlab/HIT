module Components.Navbar exposing (NavItem, view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (onClick)


type alias NavItem msg =
    { title : String
    , href : Maybe String
    , onClick : Maybe msg
    }


view :
    { title : String
    , left : List (NavItem msg)
    , right : List (NavItem msg)
    }
    -> Html msg
view props =
    Html.nav []
        [ Html.ul []
            (Html.li []
                [ Html.strong []
                    [ Html.a [ Attr.href "/" ]
                        [ Html.img [ Attr.src "/favicon.svg", Attr.alt "", Attr.width 24, Attr.height 24 ] []
                        , Html.text " "
                        , Html.text props.title
                        ]
                    ]
                ]
                :: List.map navItem props.left
            )
        , Html.ul [] (List.map navItem props.right)
        ]


navItem : NavItem msg -> Html msg
navItem item =
    let
        attrs =
            case ( item.href, item.onClick ) of
                ( Just url, Just onClickMsg ) ->
                    [ Attr.href url, onClick onClickMsg ]

                ( Just url, Nothing ) ->
                    [ Attr.href url ]

                ( Nothing, Just onClickMsg ) ->
                    [ onClick onClickMsg ]

                ( Nothing, Nothing ) ->
                    []
    in
    Html.li []
        [ Html.a attrs [ Html.text item.title ]
        ]
