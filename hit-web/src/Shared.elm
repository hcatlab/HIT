module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import Api.Auth
import Dict
import Effect exposing (Effect)
import Json.Decode
import Route exposing (Route)
import Route.Path
import Shared.Model
import Shared.Msg



-- FLAGS


type alias Flags =
    { token : Maybe String
    , user : Maybe Api.Auth.User
    }


decoder : Json.Decode.Decoder Flags
decoder =
    let
        maybeUserDecoder : Json.Decode.Decoder (Maybe Api.Auth.User)
        maybeUserDecoder =
            Json.Decode.oneOf
                [ Json.Decode.field "user" (Json.Decode.nullable Api.Auth.userDecoder)
                , Json.Decode.succeed Nothing
                ]
    in
    Json.Decode.map2 Flags
        (Json.Decode.field "token" (Json.Decode.nullable Json.Decode.string))
        maybeUserDecoder



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult _ =
    case flagsResult of
        Ok flags ->
            ( { user = flags.user, token = flags.token }
            , Effect.none
            )

        Err _ ->
            ( { user = Nothing, token = Nothing }
            , Effect.none
            )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update _ msg model =
    case msg of
        Shared.Msg.NoOp ->
            ( model
            , Effect.none
            )

        Shared.Msg.SignedInUser authResponse ->
            ( { user = Just authResponse.user
              , token = Just authResponse.token
              }
            , Effect.batch
                [ Effect.pushRoute { path = Route.Path.Goals, query = Dict.empty, hash = Nothing }
                , Effect.saveAuth authResponse
                ]
            )

        Shared.Msg.SignedOut ->
            ( { user = Nothing
              , token = Nothing
              }
            , Effect.batch
                [ Effect.pushRoute { path = Route.Path.Login, query = Dict.empty, hash = Nothing }
                , Effect.clearAuth
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
