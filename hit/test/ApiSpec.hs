{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module ApiSpec (tests) where

import Control.Applicative ((<|>))
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Value (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Vector qualified as V
import Database.PostgreSQL.Simple (Connection)
import HIT.Api (HITApiWithSwagger, HealthResponse (..))
import HIT.Api.Auth (LoginRequest (..), LoginResponse (..), SignupRequest (..), SignupResponse (..))
import HIT.DB (createUser, initDb, loginUser, lookupUserByToken, openDb)
import HIT.Handlers (goalsServer, habitsServer, intentionsServer, usersServer)
import HIT.Types.User (ApiToken (..), User, UserT (..), toPublicUser)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Test (SResponse (..))
import Servant
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import System.Environment (lookupEnv)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (request, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

type instance AuthServerData (AuthProtect "api-token") = User

mkApp :: IO Application
mkApp = do
  conn <- openDb
  initDb conn
  let ctx = authHandler conn :. EmptyContext
      api = serveWithContext (Proxy :: Proxy HITApiWithSwagger) ctx (server conn)
  pure (logStdout (simpleCors api))

server :: Connection -> Server HITApiWithSwagger
server conn = hitApi :<|> pure mempty
  where
    hitApi = health :<|> signup :<|> login :<|> me :<|> goals :<|> users :<|> habits :<|> intentions
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
  tokenText <- case extractBearerToken req of
    Nothing -> throwError err401
    Just t -> pure t
  mUser <- liftIO (lookupUserByToken conn (ApiToken tokenText))
  case mUser of
    Nothing -> throwError err403
    Just u -> pure u

extractBearerToken :: Request -> Maybe Text
extractBearerToken req = do
  let auth = lookup "Authorization" (requestHeaders req)
  case auth of
    Nothing -> Nothing
    Just raw -> case BS.stripPrefix "Bearer " raw <|> BS.stripPrefix "bearer " raw of
      Nothing -> Nothing
      Just bs -> Just (Text.decodeUtf8 bs)

tests :: IO TestTree
tests = do
  mDbUrl <- lookupEnv "DATABASE_URL"
  case mDbUrl of
    Just url | not (null url) -> testSpec "API Integration" spec
    _ -> testSpec "API Integration (skipped)" (pure ())

spec :: Spec
spec = with mkApp $ do
  describe "Health" $ do
    it "GET /health returns ok" $ do
      request "GET" "/health" [] "" `shouldRespondWith` [json| {"status":"ok"} |]

  describe "Auth and CRUD" $ do
    it "signup/login as alice" $ do
      void $ request "POST" "/signup" [("Content-Type", "application/json")] [json| {"id":"alice","email":"alice@example.com","password":"alice"} |]
      request "POST" "/login" [("Content-Type", "application/json")] [json| {"id":"alice","password":"alice"} |] `shouldRespondWith` 200

    it "create goal and read it back" $ do
      loginRes <- request "POST" "/login" [("Content-Type", "application/json")] [json| {"id":"alice","password":"alice"} |]
      let tok = extractToken loginRes
      goalRes <- request "POST" "/goals" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Build HIT","description":"Build Habit and Intention Tracker"} |]
      let gid = extractId goalRes
      readRes <- request "GET" (BS.append "/goals/" (Text.encodeUtf8 gid)) [authHeader tok] ""
      case eitherDecode (simpleBody readRes) of
        Right (Object o) -> case (KM.lookup "name" o, KM.lookup "description" o) of
          (Just (String n), Just (String d)) -> do
            liftIO $ if n == "Build HIT" && d == "Build Habit and Intention Tracker" then pure () else fail "Goal fields mismatch"
          _ -> liftIO $ fail "Goal response missing fields"
        _ -> liftIO $ fail "Invalid goal response JSON"
      listGoals <- request "GET" "/goals" [authHeader tok] ""
      case eitherDecode (simpleBody listGoals) of
        Right (Array arr) -> do
          let hasItem =
                V.any
                  ( \case
                      Object o -> KM.lookup "id" o == Just (String gid)
                      _ -> False
                  )
                  arr
          liftIO $ unless hasItem (fail "Created goal not found in /goals list")
        _ -> liftIO $ fail "Invalid /goals list response JSON"

    it "create daily habit and read it back" $ do
      loginRes <- request "POST" "/login" [("Content-Type", "application/json")] [json| {"id":"alice","password":"alice"} |]
      let tok = extractToken loginRes
      goalRes <- request "POST" "/goals" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"G","description":"D"} |]
      let gid = extractId goalRes
      habitRes <- request "POST" "/habits/daily" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Drink","description":"8 cups","sort":true,"rate":"1/1","deadline":[9,12,15],"goalIds":[#{gid}]} |]
      let hid = extractId habitRes
      readHabit <- request "GET" (BS.append "/habits/daily/" (Text.encodeUtf8 hid)) [authHeader tok] ""
      case eitherDecode (simpleBody readHabit) of
        Right (Object o) -> case (KM.lookup "name" o, KM.lookup "goalIds" o) of
          (Just (String n), Just (Array gids)) -> do
            let expected = V.singleton (String gid)
            liftIO $ if n == "Drink" && gids == expected then pure () else fail "Habit fields/goalIds mismatch"
          _ -> liftIO $ fail "Habit response missing fields"
        _ -> liftIO $ fail "Invalid habit response JSON"
      listHabits <- request "GET" "/habits?interval=daily" [authHeader tok] ""
      case eitherDecode (simpleBody listHabits) of
        Right (Array arr) -> do
          let hasItem =
                V.any
                  ( \case
                      Object o ->
                        KM.lookup "name" o == Just (String "Drink")
                          && case KM.lookup "goalIds" o of
                            Just (Array gids) -> V.any (== String gid) gids
                            _ -> False
                      _ -> False
                  )
                  arr
          liftIO $ unless hasItem (fail "Created habit not found in /habits?interval=daily list")
        _ -> liftIO $ fail "Invalid /habits list response JSON"

    it "create weekly intention and read it back" $ do
      loginRes <- request "POST" "/login" [("Content-Type", "application/json")] [json| {"id":"alice","password":"alice"} |]
      let tok = extractToken loginRes
      goalRes <- request "POST" "/goals" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"G2","description":"D2"} |]
      let gid = extractId goalRes
      intentRes <- request "POST" "/intentions/weekly" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Practice","description":"30m","rate":"1/1","deadline":{"monday":[19],"tuesday":[],"wednesday":[19],"thursday":[],"friday":[19],"saturday":[10],"sunday":[]},"goalIds":[#{gid}]} |]
      let iid = extractId intentRes
      readIntent <- request "GET" (BS.append "/intentions/weekly/" (Text.encodeUtf8 iid)) [authHeader tok] ""
      case eitherDecode (simpleBody readIntent) of
        Right (Object o) -> case (KM.lookup "name" o, KM.lookup "goalIds" o) of
          (Just (String n), Just (Array gids)) -> do
            let expected = V.singleton (String gid)
            liftIO $ if n == "Practice" && gids == expected then pure () else fail "Intention fields/goalIds mismatch"
          _ -> liftIO $ fail "Intention response missing fields"
        _ -> liftIO $ fail "Invalid intention response JSON"
      listIntent <- request "GET" "/intentions?interval=weekly" [authHeader tok] ""
      case eitherDecode (simpleBody listIntent) of
        Right (Array arr) -> do
          let hasItem =
                V.any
                  ( \case
                      Object o ->
                        KM.lookup "name" o == Just (String "Practice")
                          && case KM.lookup "goalIds" o of
                            Just (Array gids) -> V.any (== String gid) gids
                            _ -> False
                      _ -> False
                  )
                  arr
          liftIO $ unless hasItem (fail "Created intention not found in /intentions?interval=weekly list")
        _ -> liftIO $ fail "Invalid /intentions list response JSON"

-- No tests when DATABASE_URL is not set; the group is marked as (skipped)

-- Helpers
authHeader :: Text -> (HeaderName, ByteString)
authHeader tok = ("Authorization", BS.append "Bearer " (Text.encodeUtf8 tok))

extractToken :: SResponse -> Text
extractToken res =
  case eitherDecode (simpleBody res) of
    Right (Object o) -> case KM.lookup "token" o of
      Just (String t) -> t
      _ -> error "token missing"
    _ -> error "invalid login response"

extractId :: SResponse -> Text
extractId res =
  case eitherDecode (simpleBody res) of
    Right (Object o) -> case KM.lookup "id" o of
      Just (String t) -> t
      _ -> error "id missing"
    _ -> error "invalid response"
