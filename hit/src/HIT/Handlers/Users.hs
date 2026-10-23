{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.Handlers.Users
  ( usersServer,
    UsersApi,
  )
where

import Data.Text (Text)
import HIT.Api.Users (CreateUserRequest, UpdateUserRequest (..), UserResponse (..))
import HIT.Api.Users qualified as ApiUsers (UserResponse (..))
import HIT.Crud (CrudApiFor, CrudResource (..), crudServerTc)
import HIT.DB.Users (deleteUserById, getUserById, updateUserEmail)
import HIT.Env (AppM, withDb)
import HIT.Types.User (User)
import HIT.Types.User qualified as UserType (UserT (..))
import Servant
import Prelude hiding (id, read)

data UsersResource = UsersResource

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

  read _ user uid =
    if UserType.id user == uid
      then withDb $ \conn -> fmap toUserResponse <$> getUserById conn uid
      else pure Nothing

  update _ user uid (UpdateUserRequest newEmail) =
    if UserType.id user == uid
      then withDb $ \conn -> fmap toUserResponse <$> updateUserEmail conn uid newEmail
      else pure Nothing

  delete _ user uid =
    if UserType.id user == uid
      then withDb $ \conn -> deleteUserById conn uid
      else pure False

usersServer :: User -> ServerT UsersApi AppM
usersServer = crudServerTc UsersResource

toUserResponse :: User -> UserResponse
toUserResponse user = ApiUsers.UserResponse (UserType.id user) (UserType.email user) (UserType.createdAt user) (UserType.modifiedAt user)

type UsersApi = CrudApiFor UsersResource
