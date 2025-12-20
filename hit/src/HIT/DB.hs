{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB
  ( HITDb (..),
    hitDb,
    openDb,
    initDb,
    seedUser,
    createUser,
    loginUser,
    lookupUserByToken,
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
import HIT.Types.User (ApiToken (..), User, UserT (..))
import HIT.Types.User qualified as User (UserT (..))

newtype HITDb f = HITDb
  { users :: f (TableEntity UserT)
  }
  deriving (Generic)

instance Database be HITDb

hitDb :: DatabaseSettings be HITDb
hitDb =
  defaultDbSettings
    `withDbModification` dbModification
      { users =
          modifyTableFields
            ( User
                (fieldNamed "id")
                (fieldNamed "email")
                (fieldNamed "password_hash")
                (fieldNamed "api_token")
            )
      }

openDb :: FilePath -> IO Connection
openDb = Sqlite.open

initDb :: Connection -> IO ()
initDb conn = do
  let expected = ["id", "email", "password_hash", "api_token"]

  let getExistingCols :: IO [Text]
      getExistingCols =
        ( do
            rows <- Sqlite.query_ conn "PRAGMA table_info(users)" :: IO [(Int, Text, Text, Int, Maybe Text, Int)]
            pure (map (\(_, name_, _, _, _, _) -> name_) rows)
        )
          `catch` (\(_ :: Sqlite.SQLError) -> pure [])

  existing <- getExistingCols
  case existing of
    [] -> pure ()
    cols | cols == expected -> pure ()
    _ -> Sqlite.execute_ conn "DROP TABLE IF EXISTS users"

  Sqlite.execute_
    conn
    "CREATE TABLE IF NOT EXISTS users \
    \(id TEXT PRIMARY KEY, \
    \email TEXT UNIQUE NOT NULL, \
    \password_hash TEXT NOT NULL, \
    \api_token TEXT UNIQUE NOT NULL)"

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

hashPasswordText :: Text -> IO Text
hashPasswordText pw = do
  let pwBs = Text.encodeUtf8 pw
  mHashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy pwBs
  case mHashed of
    Nothing -> fail "bcrypt: failed to hash password"
    Just hashed -> pure (Text.decodeUtf8 hashed)

newToken :: IO Text
newToken = UUID.toText <$> UUIDv4.nextRandom

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

lookupUserById :: Connection -> Text -> IO (Maybe User)
lookupUserById conn uid =
  runBeamSqlite conn $
    runSelectReturningOne $
      select $ do
        u <- all_ (users hitDb)
        guard_ (User.id u ==. val_ uid)
        pure u

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
