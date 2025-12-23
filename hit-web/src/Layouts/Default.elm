module Layouts.Default exposing (Model, Msg, Props, layout)

import Components.Navbar
import Effect exposing (Effect)
import Html
import Layout exposing (Layout)
import Route exposing (Route)
import Shared
import View exposing (View)


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout _ _ _ =
    Layout.new
        { init = init
        , update = update
        , view = view
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
    = ReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ReplaceMe ->
            ( model
            , Effect.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view { content } =
    { title =
        if String.isEmpty content.title then
            "HIT"

        else
            content.title ++ " | HIT"
    , body =
        [ Html.header []
            [ Components.Navbar.view { title = "HIT", items = [ { title = "Health", href = "/health" } ] }
            ]
        , Html.main_ [] content.body
        , Html.footer []
            [ Html.small []
                [ Html.text "© 2025 HIT - Habit and Intention Tracker" ]
            ]
        ]
    }
