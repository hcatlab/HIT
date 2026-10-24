{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.App
  ( mkApp,
    server,
    authHandler,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (runReaderT)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Pool (withResource)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import HIT.API
import HIT.Config (Config (..))
import HIT.DB (createUser, loginUser, lookupUserByToken)
import HIT.Env (AppM, Env (..), withDb)
import HIT.Handlers (goalsServer, habitsServer, intentionsServer, usersServer)
import HIT.OpenAPI (hitOpenAPI)
import HIT.Types.User (APIToken (..), User (..))
import HIT.Validation (validateTimezone)
import Network.HTTP.Types.Header (hAuthorization)
import Network.Wai (Middleware, Request, pathInfo, requestHeaders)
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (..),
    cors,
    simpleCorsResourcePolicy,
  )
import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)

type instance AuthServerData (AuthProtect "api-token") = User

mkApp :: Env -> Application
mkApp env =
  logStdout
    $ stripAPIPrefix
    $ cors (corsPolicyForRequest (allowedOrigins (config env)))
    $ serveWithContextT api ctx (`runReaderT` env) server
  where
    api = Proxy :: Proxy HITAPIWithDocs
    ctx = authHandler env :. EmptyContext

-- | serve under /api too, for same-origin proxies
stripAPIPrefix :: Middleware
stripAPIPrefix next req =
  next $ case pathInfo req of
    "api" : rest -> req {pathInfo = rest}
    _ -> req

-- | localhost, 127.0.0.1 or [::1] on any port, plus configured origins
corsPolicyForRequest :: [BS.ByteString] -> Request -> Maybe CorsResourcePolicy
corsPolicyForRequest allowed req =
  case lookup "Origin" (requestHeaders req) of
    Just origin
      | isLocalOrigin origin || origin `elem` allowed ->
          Just
            ( simpleCorsResourcePolicy
                { corsOrigins = Just ([origin], True),
                  corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS", "PATCH"],
                  corsRequestHeaders = ["Authorization", "Content-Type"]
                }
            )
    _ -> Nothing

isLocalOrigin :: BS.ByteString -> Bool
isLocalOrigin o =
  any
    (`BS.isPrefixOf` o)
    [ "http://localhost",
      "https://localhost",
      "http://127.0.0.1",
      "https://127.0.0.1",
      "http://[::1]",
      "https://[::1]"
    ]

server :: ServerT HITAPIWithDocs AppM
server = hitAPI :<|> pure hitOpenAPI
  where
    hitAPI = health :<|> signup :<|> login :<|> me :<|> goalsServer :<|> usersServer :<|> habitsServer :<|> intentionsServer

    health = pure (HealthResponse "ok")

    signup (SignupRequest uid em pw mTz) = do
      tz <- maybe (pure "UTC") validateTimezone mTz
      res <- withDb $ \conn -> createUser conn uid em pw tz
      case res of
        Left msg -> throwError (err409 {errBody = LBS.fromStrict (Text.encodeUtf8 msg)})
        Right u -> pure (SignupResponse u.apiToken (toPublicUser u))

    login (LoginRequest uid pw) = do
      mUser <- withDb $ \conn -> loginUser conn uid pw
      case mUser of
        Nothing -> throwError err401
        Just u -> pure (LoginResponse u.apiToken (toPublicUser u))

    me user = pure (toPublicUser user)

authHandler :: Env -> AuthHandler Request User
authHandler env = mkAuthHandler $ \req -> do
  tokenText <-
    case extractBearerToken req of
      Nothing -> throwError err401
      Just t -> pure t
  mUser <- liftIO (withResource (pool env) (\conn -> lookupUserByToken conn (APIToken tokenText)))
  case mUser of
    Nothing -> throwError err401
    Just u -> pure u

extractBearerToken :: Request -> Maybe Text
extractBearerToken req = do
  raw <- lookup hAuthorization (requestHeaders req)
  BS.stripPrefix "Bearer " raw <|> BS.stripPrefix "bearer " raw
    >>= \bs -> Just (Text.decodeUtf8 bs)
