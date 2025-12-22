{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Handlers.Users
  ( usersServer,
    UsersApi,
  )
where

import Data.Text (Text)
import Database.PostgreSQL.Simple (Connection)
import HIT.Api.Users (CreateUserRequest (..), UpdateUserRequest (..), UserResponse (..))
import HIT.Api.Users qualified as ApiUsers (UserResponse (..))
import HIT.Crud (CrudApiFor, CrudResource (..), crudServerTc)
import HIT.DB.Users (createUser, deleteUserById, getUserById, listAllUsers, updateUserEmail)
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

  list (UsersResource conn) _ =
    map toUserResponse <$> listAllUsers conn

  create (UsersResource conn) _ (CreateUserRequest uid em pw) = do
    res <- createUser conn uid em pw
    case res of
      Left _ -> fail "failed to create user"
      Right u -> pure (toUserResponse u)

  read (UsersResource conn) _ uid =
    fmap toUserResponse <$> getUserById conn uid

  update (UsersResource conn) user uid (UpdateUserRequest newEmail) =
    if UserType.id user == uid
      then fmap toUserResponse <$> updateUserEmail conn uid newEmail
      else pure Nothing

  delete (UsersResource conn) user uid =
    if UserType.id user == uid
      then deleteUserById conn uid
      else pure False

usersServer :: Connection -> User -> Server UsersApi
usersServer conn = crudServerTc (UsersResource conn)

toUserResponse :: User -> UserResponse
toUserResponse user = ApiUsers.UserResponse (UserType.id user) (UserType.email user) (UserType.createdAt user) (UserType.modifiedAt user)

type UsersApi = CrudApiFor UsersResource
