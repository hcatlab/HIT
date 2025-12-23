module Pages.Login exposing (Model, Msg, page)

import Api
import Api.Auth
import Effect exposing (Effect)
import Html exposing (Html, button, div, form, input, label, p, text)
import Html.Attributes as Attr exposing (type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Msg as SharedMsg
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page _ _ =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout (\_ -> Layouts.Default {})



-- INIT


type alias Model =
    { id : String
    , password : String
    , loading : Bool
    , error : Maybe String
    , notice : Maybe String
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { id = ""
      , password = ""
      , loading = False
      , error = Nothing
      , notice = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UpdateId String
    | UpdatePassword String
    | Submit
    | LoginResponse (Result Http.Error Api.Auth.AuthResponse)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateId newId ->
            ( { model | id = newId }
            , Effect.none
            )

        UpdatePassword newPassword ->
            ( { model | password = newPassword }
            , Effect.none
            )

        Submit ->
            ( { model | loading = True, error = Nothing }
            , Effect.sendCmd <|
                Api.Auth.login
                    { id = model.id
                    , password = model.password
                    , onResponse = LoginResponse
                    }
            )

        LoginResponse (Ok authResponse) ->
            ( { model | loading = False, notice = Just "Login successful. Redirecting…" }
            , Effect.batch
                [ Effect.sendShared (SharedMsg.SignedInUser authResponse)
                , Effect.saveAuth authResponse
                ]
            )

        LoginResponse (Err error) ->
            ( { model
                | loading = False
                , error = Just (Api.errorToString error)
                , notice = Nothing
              }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Login"
    , body =
        [ div []
            [ Html.h2 [] [ text "Login" ]
            , form [ onSubmit Submit ]
                [ div []
                    [ label [ Attr.for "id" ] [ text "Username" ]
                    , input
                        [ Attr.id "id"
                        , type_ "text"
                        , value model.id
                        , onInput UpdateId
                        , Attr.disabled model.loading
                        ]
                        []
                    ]
                , div []
                    [ label [ Attr.for "password" ] [ text "Password" ]
                    , input
                        [ Attr.id "password"
                        , type_ "password"
                        , value model.password
                        , onInput UpdatePassword
                        , Attr.disabled model.loading
                        ]
                        []
                    ]
                , case model.error of
                    Just err ->
                        p [ Attr.style "color" "red" ] [ text err ]

                    Nothing ->
                        text ""
                , case model.notice of
                    Just msg ->
                        p [ Attr.style "color" "green" ] [ text msg ]

                    Nothing ->
                        text ""
                , button [ type_ "submit", Attr.disabled model.loading ]
                    [ text
                        (if model.loading then
                            "Logging in..."

                         else
                            "Login"
                        )
                    ]
                ]
            , p []
                [ text "Don't have an account? "
                , Html.a [ Attr.href "/signup" ] [ text "Sign up" ]
                ]
            ]
        ]
    }
