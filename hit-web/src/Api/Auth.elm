module Api.Auth exposing
    ( AuthResponse
    , User
    , login
    , signup
    , userDecoder
    )

import Api
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


type alias User =
    { id : String
    , email : String
    }


type alias AuthResponse =
    { token : String
    , user : User
    }


userDecoder : Decoder User
userDecoder =
    Decode.map2 User
        (Decode.field "id" Decode.string)
        (Decode.field "email" Decode.string)


authResponseDecoder : Decoder AuthResponse
authResponseDecoder =
    Decode.map2 AuthResponse
        (Decode.field "token" Decode.string)
        (Decode.field "user" userDecoder)


login :
    { id : String, password : String, onResponse : Result Http.Error AuthResponse -> msg }
    -> Cmd msg
login { id, password, onResponse } =
    Http.post
        { url = Api.apiUrl "/login"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "id", Encode.string id )
                    , ( "password", Encode.string password )
                    ]
        , expect = Http.expectJson onResponse authResponseDecoder
        }


signup :
    { id : String, email : String, password : String, onResponse : Result Http.Error AuthResponse -> msg }
    -> Cmd msg
signup { id, email, password, onResponse } =
    Http.post
        { url = Api.apiUrl "/signup"
        , body =
            Http.jsonBody <|
                Encode.object
                    [ ( "id", Encode.string id )
                    , ( "email", Encode.string email )
                    , ( "password", Encode.string password )
                    ]
        , expect = Http.expectJson onResponse authResponseDecoder
        }
