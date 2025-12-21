{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Database.PostgreSQL.Simple (Connection)
import HIT.Api
import HIT.DB (createUser, initDb, loginUser, lookupUserByToken, openDb)
import HIT.Handlers (goalsServer, habitsServer, intentionsServer, usersServer)
import HIT.Types.User (ApiToken (..), User, UserT (..), toPublicUser)
import Network.HTTP.Types.Header (hAuthorization)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

main :: IO ()
main = do
  putStrLn "Starting HIT server on port 8080..."
  conn <- openDb
  initDb conn

  run 8080 (app conn)

type instance AuthServerData (AuthProtect "api-token") = User

app :: Connection -> Application
app conn =
  logStdout $
    simpleCors $
      serveWithContext
        (Proxy :: Proxy HITApi)
        (authHandler conn :. EmptyContext)
        (server conn)

server :: Connection -> Server HITApi
server conn = health :<|> signup :<|> login :<|> me :<|> goals :<|> users :<|> habits :<|> intentions
  where
    health = pure (HealthResponse "ok")

    signup (SignupRequest uid em pw) = do
      res <- liftIO (createUser conn uid em pw)
      case res of
        Left msg -> throwError (err409 {errBody = LBS.fromStrict (Text.encodeUtf8 msg)})
        Right u -> pure (SignupResponse (ApiToken (apiToken u)) (toPublicUser u))

    login (LoginRequest uid pw) = do
      mUser <- liftIO (loginUser conn uid pw)
      case mUser of
        Nothing -> throwError err401
        Just u -> pure (LoginResponse (ApiToken (apiToken u)) (toPublicUser u))

    me user = pure (toPublicUser user)

    goals user = goalsServer conn user

    users user = usersServer conn user

    habits user = habitsServer conn user

    intentions user = intentionsServer conn user

authHandler :: Connection -> AuthHandler Request User
authHandler conn = mkAuthHandler $ \req -> do
  tokenText <-
    case extractBearerToken req of
      Nothing -> throwError err401
      Just t -> pure t
  mUser <- liftIO (lookupUserByToken conn (ApiToken tokenText))
  case mUser of
    Nothing -> throwError err403
    Just u -> pure u

extractBearerToken :: Request -> Maybe Text
extractBearerToken req = do
  raw <- lookup hAuthorization (requestHeaders req)
  BS.stripPrefix "Bearer " raw <|> BS.stripPrefix "bearer " raw
    >>= \bs -> Just (Text.decodeUtf8 bs)
