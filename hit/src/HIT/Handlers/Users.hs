{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Handlers.Users
  ( usersServer,
    userToday,
  )
where

import Data.Time (Day)
import HIT.API.Users (CreateUserRequest, UpdateUserRequest (..), UserResponse (..), UsersAPI)
import HIT.API.Users qualified as APIUsers (UserResponse (..))
import HIT.CRUD (CRUDResource (..), crudServer)
import HIT.DB.Users (deleteUserById, getUserById, localToday, updateUser)
import HIT.Env (AppM, withDb)
import HIT.Types.Identifier (Entity (..))
import HIT.Types.User (User)
import HIT.Types.User qualified as UserType (User (..))
import HIT.Validation (validateTimezone)
import Servant
import Prelude hiding (id, read)

instance CRUDResource 'UserEntity where
  type Label 'UserEntity = "userId"
  type CreateRequest 'UserEntity = CreateUserRequest
  type UpdateRequest 'UserEntity = UpdateUserRequest
  type Response 'UserEntity = UserResponse

  list user =
    pure [toUserResponse user]

  create _ _ =
    throwError err403 {errBody = "Use /signup to create users"}

  read user uid =
    if UserType.id user == uid
      then withDb $ \conn -> fmap toUserResponse <$> getUserById conn uid
      else pure Nothing

  update user uid (UpdateUserRequest newEmail mTz) =
    if UserType.id user == uid
      then do
        tz <- maybe (pure (UserType.timezone user)) validateTimezone mTz
        withDb $ \conn -> fmap toUserResponse <$> updateUser conn uid newEmail tz
      else pure Nothing

  delete user uid =
    if UserType.id user == uid
      then withDb $ \conn -> deleteUserById conn uid
      else pure False

usersServer :: User -> ServerT UsersAPI AppM
usersServer = crudServer @'UserEntity

-- | the user's current date in their timezone
userToday :: User -> AppM Day
userToday user = withDb $ \conn -> localToday conn (UserType.timezone user)

toUserResponse :: User -> UserResponse
toUserResponse user = APIUsers.UserResponse (UserType.id user) (UserType.email user) (UserType.timezone user) (UserType.createdAt user) (UserType.modifiedAt user)
