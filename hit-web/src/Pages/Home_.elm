module Pages.Home_ exposing (Model, Msg, page)

import Effect exposing (Effect)
import Html
import Html.Attributes as Attr
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Shared
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
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type alias Msg =
    Never


update : Msg -> Model -> ( Model, Effect Msg )
update msg _ =
    never msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> View Msg
view _ =
    { title = "Home"
    , body =
        [ Html.article [ Attr.attribute "role" "note" ]
            [ Html.mark [] [ Html.text "Warning" ]
            , Html.text " This site is in alpha for testing, and data may be dropped at any time."
            ]
        , Html.blockquote []
            [ Html.text "To achieve great things, two things are needed: a plan, and not quite enough time."
            , Html.br [] []
            , Html.text "— Leonard Bernstein"
            ]
        ]
    }
