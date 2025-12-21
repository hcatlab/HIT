{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Users
  ( seedUser,
    createUser,
    loginUser,
    lookupUserByToken,
    listAllUsers,
    getUserById,
    updateUserEmail,
    deleteUserById,
  )
where

import Control.Exception (catch)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.ByteString.Char8 qualified as BS8
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam
import Database.Beam.Sqlite (runBeamSqlite)
import Database.SQLite.Simple (Connection)
import Database.SQLite.Simple qualified as Sqlite
import HIT.DB.Schema (HITDb (..), hitDb)
import HIT.Types.User (ApiToken (..), User, UserT (..))
import HIT.Types.User qualified as User (UserT (..))

seedUser :: Connection -> User -> IO ()
seedUser conn u@(User uid uemail _ utoken) = do
  mExisting <-
    runBeamSqlite conn $
      runSelectReturningOne $
        select $ do
          row <- all_ (users hitDb)
          guard_
            ( User.id row
                ==. val_ uid
                ||. email row
                ==. val_ uemail
                ||. apiToken row
                ==. val_ utoken
            )
          pure row

  case mExisting of
    Just _ -> pure ()
    Nothing ->
      runBeamSqlite conn $
        runInsert $
          insert (users hitDb) $
            insertValues [u]

createUser :: Connection -> Text -> Text -> Text -> IO (Either Text User)
createUser conn username_ email_ password_ = do
  tok <- newToken
  pwHash <- hashPasswordText password_
  let u = User username_ email_ pwHash tok
  ( Right <$> do
      runBeamSqlite conn $
        runInsert $
          insert (users hitDb) $
            insertValues [u]
      pure u
    )
    `catch` \(e :: Sqlite.SQLError) ->
      if Sqlite.sqlError e == Sqlite.ErrorConstraint
        then pure (Left "username or email already exists")
        else pure (Left (Text.decodeUtf8 (BS8.pack (show e))))

loginUser :: Connection -> Text -> Text -> IO (Maybe User)
loginUser conn uname password_ = do
  mUser <- lookupUserById conn uname
  case mUser of
    Nothing -> pure Nothing
    Just u@(User _ _ pwHash _) -> do
      let ok =
            validatePassword
              (Text.encodeUtf8 pwHash)
              (Text.encodeUtf8 password_)
      if not ok
        then pure Nothing
        else do
          tok <- newToken
          runBeamSqlite conn $
            runUpdate $
              update
                (users hitDb)
                (\row -> apiToken row <-. val_ tok)
                (\row -> User.id row ==. val_ uname)
          pure (Just (u {apiToken = tok}))

lookupUserByToken :: Connection -> ApiToken -> IO (Maybe User)
lookupUserByToken conn (ApiToken tok) =
  runBeamSqlite conn $
    runSelectReturningOne $
      select $ do
        u <- all_ (users hitDb)
        guard_ (apiToken u ==. val_ tok)
        pure u

-- User CRUD operations

listAllUsers :: Connection -> IO [User]
listAllUsers conn =
  runBeamSqlite conn $
    runSelectReturningList $
      select (all_ (users hitDb))

getUserById :: Connection -> Text -> IO (Maybe User)
getUserById = lookupUserById

updateUserEmail :: Connection -> Text -> Text -> IO (Maybe User)
updateUserEmail conn uid newEmail = do
  mExisting <- lookupUserById conn uid
  case mExisting of
    Nothing -> pure Nothing
    Just (User _ _ pwHash tok) -> do
      runBeamSqlite conn $
        runUpdate $
          update
            (users hitDb)
            (\u -> email u <-. val_ newEmail)
            (\u -> User.id u ==. val_ uid)
      pure (Just (User uid newEmail pwHash tok))

deleteUserById :: Connection -> Text -> IO Bool
deleteUserById conn uid = do
  mExisting <- lookupUserById conn uid
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamSqlite conn $
        runDelete $
          delete (users hitDb) (\u -> User.id u ==. val_ uid)
      pure True

-- Internal helpers

lookupUserById :: Connection -> Text -> IO (Maybe User)
lookupUserById conn uid =
  runBeamSqlite conn $
    runSelectReturningOne $
      select $ do
        u <- all_ (users hitDb)
        guard_ (User.id u ==. val_ uid)
        pure u

hashPasswordText :: Text -> IO Text
hashPasswordText pw = do
  let pwBs = Text.encodeUtf8 pw
  mHashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy pwBs
  case mHashed of
    Nothing -> fail "bcrypt: failed to hash password"
    Just hashed -> pure (Text.decodeUtf8 hashed)

newToken :: IO Text
newToken = UUID.toText <$> UUIDv4.nextRandom
