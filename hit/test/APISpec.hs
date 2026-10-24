{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module APISpec (tests) where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Value (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Pool (withResource)
import Data.Text (Text, unpack)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Vector qualified as V
import HIT.App (mkApp)
import HIT.Config (loadConfig)
import HIT.DB (migrate)
import HIT.Env (Env (..), newEnv)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Application)
import Network.Wai.Test (SResponse (..))
import System.Environment (lookupEnv)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Wai (request, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (testSpec)

mkTestApp :: IO Application
mkTestApp = do
  env <- loadConfig >>= newEnv
  withResource (pool env) migrate
  pure (mkApp env)

-- | skipped without DATABASE_URL
tests :: IO TestTree
tests = do
  mDbUrl <- lookupEnv "DATABASE_URL"
  case mDbUrl of
    Just url | not (null url) -> testSpec "API Integration" spec
    _ -> testSpec "API Integration (skipped)" (pure ())

spec :: Spec
spec = with mkTestApp $ do
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
      -- update goal, modifiedAt must increase
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

    it "malformed identifiers and refined values are rejected before the handlers" $ do
      loginRes <- request "POST" "/login" [("Content-Type", "application/json")] [json| {"id":"alice","password":"alice"} |]
      let tok = extractToken loginRes
      request "GET" "/goals/not-a-uuid" [authHeader tok] "" `shouldRespondWith` 400
      request "POST" "/intentions" [authHeader tok, ("Content-Type", "application/json")] [json| {"title":"x","goalIds":["not-a-uuid"]} |] `shouldRespondWith` 400
      request "POST" "/intentions" [authHeader tok, ("Content-Type", "application/json")] [json| {"title":"   ","goalIds":["00000000-0000-0000-0000-000000000000"]} |] `shouldRespondWith` 400
      request "POST" "/intentions" [authHeader tok, ("Content-Type", "application/json")] [json| {"title":"x","target":0,"goalIds":["00000000-0000-0000-0000-000000000000"]} |] `shouldRespondWith` 400

    it "habits and intentions are created, listed and checked off" $ do
      loginRes <- request "POST" "/login" [("Content-Type", "application/json")] [json| {"id":"alice","password":"alice"} |]
      let tok = extractToken loginRes
          post path = request "POST" path [authHeader tok, ("Content-Type", "application/json")]
      goalRes <- post "/goals" [json| {"name":"Run","description":"Run more","color":"#017290","startDate":null,"endDate":null} |]
      let gid = extractId goalRes
      post "/habits" [json| {"title":"run 5km","target":5,"unit":"km","interval":"daily","weekdays":["wednesday","friday"],"dueTime":"18:00:00","goalIds":[#{gid}]} |] `shouldRespondWith` 200
      post "/habits" [json| {"title":"run 3 times","target":3,"interval":"weekly","goalIds":[#{gid}]} |] `shouldRespondWith` 200
      post "/habits" [json| {"title":"bad","interval":"weekly","weekdays":["monday"],"goalIds":[#{gid}]} |] `shouldRespondWith` 400
      habits <- titlesOf <$> request "GET" "/habits" [authHeader tok] ""
      liftIO $ unless (all (`elem` habits) ["run 5km", "run 3 times"]) (fail ("missing habits: " <> show habits))
      intentRes <- post "/intentions" [json| {"title":"finish reading","dueTime":"16:00:00","goalIds":[#{gid}]} |]
      let iid = extractId intentRes
      intentions <- titlesOf <$> request "GET" "/intentions" [authHeader tok] ""
      liftIO $ unless ("finish reading" `elem` intentions) (fail ("missing intention: " <> show intentions))
      -- check off
      doneRes <- request "PUT" (BS.append "/intentions/" (Text.encodeUtf8 iid)) [authHeader tok, ("Content-Type", "application/json")] [json| {"title":"finish reading","target":1,"dueTime":"16:00:00","progress":1,"goalIds":[#{gid}]} |]
      liftIO $ case eitherDecode (simpleBody doneRes) of
        Right (Object o) | Just (String _) <- KM.lookup "doneAt" o -> pure ()
        other -> fail ("intention not done: " <> show other)
      -- goals with habits or intentions are kept
      request "DELETE" (BS.append "/goals/" (Text.encodeUtf8 gid)) [authHeader tok] "" `shouldRespondWith` 409

-- helpers
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

-- | titles of a json array of objects
titlesOf :: SResponse -> [Text]
titlesOf res =
  case eitherDecode (simpleBody res) of
    Right (Array xs) -> [t | Object o <- V.toList xs, Just (String t) <- [KM.lookup "title" o]]
    _ -> error "invalid list response"
