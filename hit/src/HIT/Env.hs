module HIT.Env
  ( Env (..),
    AppM,
    newEnv,
    withDb,
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, asks)
import Data.Pool (Pool, defaultPoolConfig, newPool, withResource)
import Database.PostgreSQL.Simple (Connection, close, connectPostgreSQL)
import HIT.Config (Config (..))
import Servant (Handler)

data Env = Env
  { pool :: Pool Connection,
    config :: Config
  }

type AppM = ReaderT Env Handler

-- | pool with 60s idle timeout
newEnv :: Config -> IO Env
newEnv cfg = do
  p <- newPool (defaultPoolConfig (connectPostgreSQL (databaseUrl cfg)) close 60 (poolSize cfg))
  pure (Env p cfg)

-- | one pooled connection per call, dropped on exception
withDb :: (Connection -> IO a) -> AppM a
withDb act = do
  p <- asks pool
  liftIO (withResource p act)
