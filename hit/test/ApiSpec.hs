{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

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
import Data.Text (Text, unpack)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
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
    goals = goalsServer conn
    users = usersServer conn
    habits = habitsServer conn
    intentions = intentionsServer conn

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
      goalRes <- request "POST" "/goals" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Build HIT","description":"Build Habit and Intention Tracker","color":"#017290","startDate":null,"endDate":null} |]
      let gid = extractId goalRes
      readRes <- request "GET" (BS.append "/goals/" (Text.encodeUtf8 gid)) [authHeader tok] ""
      (createdAt, modifiedAt) <- case eitherDecode (simpleBody readRes) of
        Right (Object o) -> case (KM.lookup "name" o, KM.lookup "description" o, KM.lookup "createdAt" o, KM.lookup "modifiedAt" o) of
          (Just (String n), Just (String d), Just (String c), Just (String m)) -> do
            liftIO $ if n == "Build HIT" && d == "Build Habit and Intention Tracker" then pure () else fail "Goal fields mismatch"
            let parseTS = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . unpack :: Text -> Maybe UTCTime
            ct <- maybe (fail "createdAt parse fail") pure (parseTS c)
            mt <- maybe (fail "modifiedAt parse fail") pure (parseTS m)
            liftIO $ if ct == mt then pure () else fail "createdAt and modifiedAt should be equal on create"
            pure (ct, mt)
          _ -> liftIO (fail "Goal response missing fields")
        _ -> liftIO (fail "Invalid goal response JSON")
      -- Update goal and check modifiedAt changes
      _ <- request "PUT" (BS.append "/goals/" (Text.encodeUtf8 gid)) [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Build HIT+","description":"Updated desc","color":"#017290","startDate":"2023-01-01","endDate":null} |]
      updRes <- request "GET" (BS.append "/goals/" (Text.encodeUtf8 gid)) [authHeader tok] ""
      case eitherDecode (simpleBody updRes) of
        Right (Object o) -> case (KM.lookup "createdAt" o, KM.lookup "modifiedAt" o) of
          (Just (String c2), Just (String m2)) -> do
            let parseTS = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . unpack :: Text -> Maybe UTCTime
            ct2 <- maybe (fail "createdAt parse fail (upd)") pure (parseTS c2)
            mt2 <- maybe (fail "modifiedAt parse fail (upd)") pure (parseTS m2)
            liftIO $ if ct2 == createdAt then pure () else fail "createdAt should not change on update"
            liftIO $ if mt2 > modifiedAt then pure () else fail "modifiedAt should increase on update"
          _ -> liftIO (fail "Goal update response missing timestamps")
        _ -> liftIO (fail "Invalid goal update response JSON")
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
      goalRes <- request "POST" "/goals" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"G","description":"D","color":"#017290","startDate":null,"endDate":null} |]
      let gid = extractId goalRes
      habitRes <- request "POST" "/habits/daily" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Drink","description":"8 cups","sort":true,"rate":"1/1","deadline":[9,12,15],"goalIds":[#{gid}]} |]
      let hid = extractId habitRes
      readHabit <- request "GET" (BS.append "/habits/daily/" (Text.encodeUtf8 hid)) [authHeader tok] ""
      (createdAt, modifiedAt) <- case eitherDecode (simpleBody readHabit) of
        Right (Object o) -> case (KM.lookup "name" o, KM.lookup "goalIds" o, KM.lookup "createdAt" o, KM.lookup "modifiedAt" o) of
          (Just (String n), Just (Array gids), Just (String c), Just (String m)) -> do
            let expected = V.singleton (String gid)
            liftIO $ if n == "Drink" && gids == expected then pure () else fail "Habit fields/goalIds mismatch"
            let parseTS = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . unpack :: Text -> Maybe UTCTime
            ct <- maybe (fail "createdAt parse fail") pure (parseTS c)
            mt <- maybe (fail "modifiedAt parse fail") pure (parseTS m)
            liftIO $ if ct == mt then pure () else fail "createdAt and modifiedAt should be equal on create (habit)"
            pure (ct, mt)
          _ -> liftIO (fail "Habit response missing fields")
        _ -> liftIO (fail "Invalid habit response JSON")
      -- Update habit and check modifiedAt changes
      _ <- request "PUT" (BS.append "/habits/daily/" (Text.encodeUtf8 hid)) [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Drink+","description":"8 cups+","sort":true,"rate":"1/1","deadline":[10,12,15],"goalIds":[#{gid}]} |]
      updRes <- request "GET" (BS.append "/habits/daily/" (Text.encodeUtf8 hid)) [authHeader tok] ""
      case eitherDecode (simpleBody updRes) of
        Right (Object o) -> case (KM.lookup "createdAt" o, KM.lookup "modifiedAt" o) of
          (Just (String c2), Just (String m2)) -> do
            let parseTS = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . unpack :: Text -> Maybe UTCTime
            ct2 <- maybe (fail "createdAt parse fail (upd)") pure (parseTS c2)
            mt2 <- maybe (fail "modifiedAt parse fail (upd)") pure (parseTS m2)
            liftIO $ if ct2 == createdAt then pure () else fail "createdAt should not change on update (habit)"
            liftIO $ if mt2 > modifiedAt then pure () else fail "modifiedAt should increase on update (habit)"
          _ -> liftIO (fail "Habit update response missing timestamps")
        _ -> liftIO (fail "Invalid habit update response JSON")
      -- List check remains unchanged
      listHabits <- request "GET" "/habits?interval=daily" [authHeader tok] ""
      case eitherDecode (simpleBody listHabits) of
        Right (Array arr) -> do
          let hasItem =
                V.any
                  ( \case
                      Object o ->
                        KM.lookup "name" o == Just (String "Drink+")
                          && case KM.lookup "goalIds" o of
                            Just (Array gids) -> V.any (== String gid) gids
                            _ -> False
                      _ -> False
                  )
                  arr
          liftIO $ unless hasItem (fail "Updated habit not found in /habits?interval=daily list")
        _ -> liftIO $ fail "Invalid /habits list response JSON"

    it "create weekly intention and read it back" $ do
      loginRes <- request "POST" "/login" [("Content-Type", "application/json")] [json| {"id":"alice","password":"alice"} |]
      let tok = extractToken loginRes
      goalRes <- request "POST" "/goals" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"G2","description":"D2","color":"#017290","startDate":null,"endDate":null} |]
      let gid = extractId goalRes
      intentRes <- request "POST" "/intentions/weekly" [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Practice","description":"30m","rate":"1/1","deadline":{"monday":[19],"tuesday":[],"wednesday":[19],"thursday":[],"friday":[19],"saturday":[10],"sunday":[]},"goalIds":[#{gid}]} |]
      let iid = extractId intentRes
      readIntent <- request "GET" (BS.append "/intentions/weekly/" (Text.encodeUtf8 iid)) [authHeader tok] ""
      (createdAt, modifiedAt) <- case eitherDecode (simpleBody readIntent) of
        Right (Object o) -> case (KM.lookup "name" o, KM.lookup "goalIds" o, KM.lookup "createdAt" o, KM.lookup "modifiedAt" o) of
          (Just (String n), Just (Array gids), Just (String c), Just (String m)) -> do
            let expected = V.singleton (String gid)
            liftIO $ if n == "Practice" && gids == expected then pure () else fail "Intention fields/goalIds mismatch"
            let parseTS = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . unpack :: Text -> Maybe UTCTime
            ct <- maybe (fail "createdAt parse fail") pure (parseTS c)
            mt <- maybe (fail "modifiedAt parse fail") pure (parseTS m)
            liftIO $ if ct == mt then pure () else fail "createdAt and modifiedAt should be equal on create (intention)"
            pure (ct, mt)
          _ -> liftIO (fail "Intention response missing fields")
        _ -> liftIO (fail "Invalid intention response JSON")
      -- Update intention and check modifiedAt changes
      _ <- request "PUT" (BS.append "/intentions/weekly/" (Text.encodeUtf8 iid)) [authHeader tok, ("Content-Type", "application/json")] [json| {"name":"Practice+","description":"31m","rate":"1/1","deadline":{"monday":[20],"tuesday":[],"wednesday":[20],"thursday":[],"friday":[20],"saturday":[11],"sunday":[]},"goalIds":[#{gid}]} |]
      updRes <- request "GET" (BS.append "/intentions/weekly/" (Text.encodeUtf8 iid)) [authHeader tok] ""
      case eitherDecode (simpleBody updRes) of
        Right (Object o) -> case (KM.lookup "createdAt" o, KM.lookup "modifiedAt" o) of
          (Just (String c2), Just (String m2)) -> do
            let parseTS = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S%QZ" . unpack :: Text -> Maybe UTCTime
            ct2 <- maybe (fail "createdAt parse fail (upd)") pure (parseTS c2)
            mt2 <- maybe (fail "modifiedAt parse fail (upd)") pure (parseTS m2)
            liftIO $ if ct2 == createdAt then pure () else fail "createdAt should not change on update (intention)"
            liftIO $ if mt2 > modifiedAt then pure () else fail "modifiedAt should increase on update (intention)"
          _ -> liftIO (fail "Intention update response missing timestamps")
        _ -> liftIO (fail "Invalid intention update response JSON")
      -- List check remains unchanged
      listIntent <- request "GET" "/intentions?interval=weekly" [authHeader tok] ""
      case eitherDecode (simpleBody listIntent) of
        Right (Array arr) -> do
          let hasItem =
                V.any
                  ( \case
                      Object o ->
                        KM.lookup "name" o == Just (String "Practice+")
                          && case KM.lookup "goalIds" o of
                            Just (Array gids) -> V.any (== String gid) gids
                            _ -> False
                      _ -> False
                  )
                  arr
          liftIO $ unless hasItem (fail "Updated intention not found in /intentions?interval=weekly list")
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
