{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Users
  ( createUser,
    loginUser,
    lookupUserByToken,
    getUserById,
    updateUser,
    deleteUserById,
    localToday,
    validTimezone,
  )
where

import Control.Exception (catch, throwIO)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time (Day, getCurrentTime)
import Data.UUID qualified as UUID
import Data.UUID.V4 qualified as UUIDv4
import Database.Beam
import Database.Beam.Postgres (runBeamPostgres)
import Database.PostgreSQL.Simple (Connection, Only (..))
import Database.PostgreSQL.Simple qualified as PG
import HIT.DB.Codec (decodeOrThrow, decodeUser, encodeUser)
import HIT.DB.Rows qualified as Row (UserRow, UserRowT (..))
import HIT.DB.Schema (HITDb (..), hitDb)
import HIT.Types.Identifier (Username)
import HIT.Types.User (APIToken (..), User (..))

createUser :: Connection -> Username -> Text -> Text -> Text -> IO (Either Text User)
createUser conn username_ email_ password_ tz = do
  tok <- newToken
  pwHash <- hashPasswordText password_
  now <- getCurrentTime
  let u = User username_ email_ pwHash tok tz now now
  ( Right <$> do
      runBeamPostgres conn
        $ runInsert
        $ insert (users hitDb)
        $ insertValues [encodeUser u]
      pure u
    )
    `catch` \(e :: PG.SqlError) ->
      -- unique violation
      if PG.sqlState e == "23505"
        then pure (Left "username or email already exists")
        else throwIO e

loginUser :: Connection -> Username -> Text -> IO (Maybe User)
loginUser conn uname password_ = do
  mUser <- getUserById conn uname
  case mUser of
    Just u | validatePassword (Text.encodeUtf8 u.passwordHash) (Text.encodeUtf8 password_) -> do
      tok <- newToken
      let u' = u {apiToken = tok}
      runBeamPostgres conn $ runUpdate $ save (users hitDb) (encodeUser u')
      pure (Just u')
    _ -> pure Nothing

lookupUserByToken :: Connection -> APIToken -> IO (Maybe User)
lookupUserByToken conn tok =
  decodeOne $
    runBeamPostgres conn $
      runSelectReturningOne $
        select $ do
          u <- all_ (users hitDb)
          guard_ (Row.apiToken u ==. val_ tok)
          pure u

getUserById :: Connection -> Username -> IO (Maybe User)
getUserById conn uid =
  decodeOne $
    runBeamPostgres conn $
      runSelectReturningOne $
        select $ do
          u <- all_ (users hitDb)
          guard_ (Row.id u ==. val_ uid)
          pure u

-- | new email and timezone
updateUser :: Connection -> Username -> Text -> Text -> IO (Maybe User)
updateUser conn uid newEmail tz = do
  mExisting <- getUserById conn uid
  case mExisting of
    Nothing -> pure Nothing
    Just old -> do
      now <- getCurrentTime
      let u = old {email = newEmail, timezone = tz, modifiedAt = now}
      runBeamPostgres conn $ runUpdate $ save (users hitDb) (encodeUser u)
      pure (Just u)

deleteUserById :: Connection -> Username -> IO Bool
deleteUserById conn uid = do
  mExisting <- getUserById conn uid
  case mExisting of
    Nothing -> pure False
    Just _ -> do
      runBeamPostgres conn
        $ runDelete
        $ delete (users hitDb) (\u -> Row.id u ==. val_ uid)
      pure True

-- | current date in an iana timezone
localToday :: Connection -> Text -> IO Day
localToday conn tz = do
  rows <- PG.query conn "SELECT (now() AT TIME ZONE ?)::date" (Only tz)
  case rows of
    [Only d] -> pure d
    _ -> fail "localToday: no row"

validTimezone :: Connection -> Text -> IO Bool
validTimezone conn tz = do
  rows <- PG.query conn "SELECT EXISTS (SELECT 1 FROM pg_timezone_names WHERE name = ?)" (Only tz)
  pure (rows == [Only True])

-- internal helpers

decodeOne :: IO (Maybe Row.UserRow) -> IO (Maybe User)
decodeOne query = query >>= traverse (decodeOrThrow . decodeUser)

hashPasswordText :: Text -> IO Text
hashPasswordText pw = do
  let pwBs = Text.encodeUtf8 pw
  mHashed <- hashPasswordUsingPolicy slowerBcryptHashingPolicy pwBs
  case mHashed of
    Nothing -> fail "bcrypt: failed to hash password"
    Just hashed -> pure (Text.decodeUtf8 hashed)

newToken :: IO APIToken
newToken = APIToken . UUID.toText <$> UUIDv4.nextRandom
