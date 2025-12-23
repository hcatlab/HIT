module Layouts.Default exposing (Model, Msg, Props, layout)

import Components.Navbar exposing (NavItem)
import Effect exposing (Effect)
import Html
import Html.Attributes
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Msg as SharedMsg
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ shared route =
    Layout.new
        { init = init
        , update = update
        , view = view shared route
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = Logout


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        Logout ->
            ( model
            , Effect.batch
                [ Effect.sendShared SharedMsg.SignedOut
                , Effect.clearAuth
                ]
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Route () -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view shared route { toContentMsg, content } =
    let
        navLink title href =
            { title = title, href = Just href, onClick = Nothing }

        navAction title msg =
            { title = title, href = Nothing, onClick = Just msg }

        navbarProps : { title : String, left : List (NavItem Msg), right : List (NavItem Msg) }
        navbarProps =
            case shared.user of
                Just user ->
                    { title = "HIT"
                    , left = [ navLink "Goals" "/goals" ]
                    , right = [ navLink user.id "/profile", navAction "Logout" Logout ]
                    }

                Nothing ->
                    { title = "HIT"
                    , left = []
                    , right = [ navLink "Login" "/login", navLink "Sign up" "/signup" ]
                    }

        currentPath : String
        currentPath =
            Route.Path.toString route.path
    in
    { title =
        if String.isEmpty content.title then
            "HIT"

        else
            content.title ++ " | HIT"
    , body =
        [ Html.header []
            [ Components.Navbar.view navbarProps currentPath
                |> Html.map toContentMsg
            ]
        , Html.main_ [] content.body
        , Html.footer []
            [ Html.div [ Html.Attributes.class "footer-bar" ]
                [ Html.div [ Html.Attributes.class "footer-left" ] []
                , Html.div [ Html.Attributes.class "footer-center" ]
                    [ Html.small [] [ Html.text "© 2025 HIT Authors" ] ]
                , Html.div [ Html.Attributes.class "footer-right" ]
                    [ Html.small []
                        [ Html.a [ Html.Attributes.href "/health" ] [ Html.text "Health" ]
                        , Html.text " "
                        , Html.a [ Html.Attributes.href "https://github.com/DesyncTheThird/HIT" ] [ Html.text "Source" ]
                        ]
                    ]
                ]
            ]
        ]
    }
