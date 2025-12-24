module Components.Listing exposing (Model, view)

import Html exposing (Html, div)
import Html.Attributes exposing (class)


type alias Model a msg =
    { items : List a
    , render : a -> Html msg
    , listClass : String
    , itemClass : String
    }


view : Model a msg -> Html msg
view model =
    div [ class model.listClass ]
        (List.map
            (\item ->
                div [ class model.itemClass ] [ model.render item ]
            )
            model.items
        )
