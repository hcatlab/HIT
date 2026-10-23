{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Handlers.Users
  ( usersServer,
    UsersApi,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import HIT.Api.Users (CreateUserRequest, UpdateUserRequest (..), UserResponse (..))
import HIT.Api.Users qualified as ApiUsers (UserResponse (..))
import HIT.Crud (CrudApiFor, CrudResource (..), crudServerTc)
import HIT.DB.Users (deleteUserById, getUserById, updateUserEmail)
import HIT.Types.User (User)
import HIT.Types.User qualified as UserType (UserT (..))
import Servant
import Prelude hiding (id, read)

newtype UsersResource = UsersResource Connection

instance CrudResource UsersResource where
  type Label UsersResource = "userId"
  type InternalId UsersResource = Text
  type CreateReq UsersResource = CreateUserRequest
  type UpdateReq UsersResource = UpdateUserRequest
  type Resp UsersResource = UserResponse

  parseId _ = Just

  list _ user =
    pure [toUserResponse user]

  create _ _ _ =
    throwError err403 {errBody = "Use /signup to create users"}

  read (UsersResource conn) user uid =
    if UserType.id user == uid
      then liftIO $ fmap toUserResponse <$> getUserById conn uid
      else pure Nothing

  update (UsersResource conn) user uid (UpdateUserRequest newEmail) =
    if UserType.id user == uid
      then liftIO $ fmap toUserResponse <$> updateUserEmail conn uid newEmail
      else pure Nothing

  delete (UsersResource conn) user uid =
    if UserType.id user == uid
      then liftIO $ deleteUserById conn uid
      else pure False

usersServer :: Connection -> User -> Server UsersApi
usersServer conn = crudServerTc (UsersResource conn)

toUserResponse :: User -> UserResponse
toUserResponse user = ApiUsers.UserResponse (UserType.id user) (UserType.email user) (UserType.createdAt user) (UserType.modifiedAt user)

type UsersApi = CrudApiFor UsersResource
