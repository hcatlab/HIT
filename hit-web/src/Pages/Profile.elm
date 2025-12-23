module Pages.Profile exposing (Model, Msg, page)

import Dict
import Effect exposing (Effect)
import Html exposing (button, div, p, strong, text)
import Html.Events exposing (onClick)
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model as SharedModel
import Shared.Msg as SharedMsg
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
    { user : Maybe SharedModel.User }


init : Shared.Model -> () -> ( Model, Effect Msg )
init shared _ =
    case shared.user of
        Just user ->
            ( { user = Just user }
            , Effect.none
            )

        Nothing ->
            ( { user = Nothing }
            , Effect.pushRoute { path = Route.Path.Login, query = Dict.empty, hash = Nothing }
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
                , Effect.pushRoute { path = Route.Path.Login, query = Dict.empty, hash = Nothing }
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Profile"
    , body =
        case model.user of
            Nothing ->
                [ p [] [ text "Redirecting to login..." ] ]

            Just user ->
                [ div []
                    [ p [] [ strong [] [ text "User" ], text (": " ++ user.id) ]
                    , p [] [ strong [] [ text "Email" ], text (": " ++ user.email) ]
                    , button [ onClick Logout ] [ text "Logout" ]
                    ]
                ]
    }
