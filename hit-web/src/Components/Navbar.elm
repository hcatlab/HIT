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
    -> String
    -> Html msg
view props currentPath =
    Html.nav []
        [ Html.ul []
            (Html.li []
                [ Html.strong []
                    [ Html.a [ Attr.href "/" ]
                        [ Html.img
                            [ Attr.src "/favicon.svg"
                            , Attr.alt "Logo"
                            , Attr.width 48
                            , Attr.height 48
                            ]
                            []
                        , Html.text " "
                        , Html.text props.title
                        ]
                    ]
                ]
                :: List.map (navItem currentPath) props.left
            )
        , Html.ul [] (List.map (navItem currentPath) props.right)
        ]


navItem : String -> NavItem msg -> Html msg
navItem currentPath item =
    let
        isCurrent =
            case item.href of
                Just url ->
                    url == currentPath

                Nothing ->
                    False
    in
    Html.li []
        (if isCurrent then
            [ Html.button [] [ Html.text item.title ] ]

         else
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
            [ Html.a attrs [ Html.text item.title ] ]
        )
