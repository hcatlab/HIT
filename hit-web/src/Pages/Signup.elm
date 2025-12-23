module Pages.Signup exposing (Model, Msg, page)

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
    , email : String
    , password : String
    , confirmPassword : String
    , loading : Bool
    , error : Maybe String
    }


init : () -> ( Model, Effect Msg )
init _ =
    ( { id = ""
      , email = ""
      , password = ""
      , confirmPassword = ""
      , loading = False
      , error = Nothing
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = UpdateId String
    | UpdateEmail String
    | UpdatePassword String
    | UpdateConfirmPassword String
    | Submit
    | SignupResponse (Result Http.Error Api.Auth.AuthResponse)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        UpdateId newId ->
            ( { model | id = newId }
            , Effect.none
            )

        UpdateEmail newEmail ->
            ( { model | email = newEmail }
            , Effect.none
            )

        UpdatePassword newPassword ->
            ( { model | password = newPassword }
            , Effect.none
            )

        UpdateConfirmPassword newConfirm ->
            ( { model | confirmPassword = newConfirm }
            , Effect.none
            )

        Submit ->
            if model.password /= model.confirmPassword then
                ( { model | error = Just "Passwords do not match" }
                , Effect.none
                )

            else if String.isEmpty model.id || String.isEmpty model.email || String.isEmpty model.password then
                ( { model | error = Just "All fields are required" }
                , Effect.none
                )

            else
                ( { model | loading = True, error = Nothing }
                , Effect.sendCmd <|
                    Api.Auth.signup
                        { id = model.id
                        , email = model.email
                        , password = model.password
                        , onResponse = SignupResponse
                        }
                )

        SignupResponse (Ok authResponse) ->
            ( { model | loading = False }
            , Effect.batch
                [ Effect.sendShared (SharedMsg.SignedInUser authResponse)
                , Effect.saveAuth authResponse
                ]
            )

        SignupResponse (Err error) ->
            ( { model
                | loading = False
                , error = Just (Api.errorToString error)
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
    { title = "Sign Up"
    , body =
        [ div []
            [ Html.h2 [] [ text "Sign Up" ]
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
                    [ label [ Attr.for "email" ] [ text "Email" ]
                    , input
                        [ Attr.id "email"
                        , type_ "email"
                        , value model.email
                        , onInput UpdateEmail
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
                , div []
                    [ label [ Attr.for "confirm" ] [ text "Confirm Password" ]
                    , input
                        [ Attr.id "confirm"
                        , type_ "password"
                        , value model.confirmPassword
                        , onInput UpdateConfirmPassword
                        , Attr.disabled model.loading
                        ]
                        []
                    ]
                , case model.error of
                    Just err ->
                        p [ Attr.style "color" "red" ] [ text err ]

                    Nothing ->
                        text ""
                , button [ type_ "submit", Attr.disabled model.loading ]
                    [ text
                        (if model.loading then
                            "Creating account..."

                         else
                            "Sign Up"
                        )
                    ]
                ]
            , p []
                [ text "Already have an account? "
                , Html.a [ Attr.href "/login" ] [ text "Login" ]
                ]
            ]
        ]
    }
