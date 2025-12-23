module Auth exposing (User, onPageLoad, viewCustomPage)

import Auth.Action
import Dict
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model as SharedModel
import View exposing (View)


type alias User =
    SharedModel.User


{-| Called before an auth-only page is loaded.
If the user is not logged in, redirect to login page.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.user of
        Just user ->
            Auth.Action.loadPageWithUser user

        Nothing ->
            Auth.Action.pushRoute
                { path = Route.Path.Login
                , query = Dict.empty
                , hash = Nothing
                }


{-| Renders whenever `Auth.Action.loadCustomPage` is returned from `onPageLoad`.
-}
viewCustomPage : Shared.Model -> Route () -> View Never
viewCustomPage shared route =
    View.fromString "Loading..."
