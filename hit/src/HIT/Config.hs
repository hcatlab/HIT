module HIT.Config
  ( Config (..),
    loadConfig,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Config = Config
  { port :: Int,
    databaseUrl :: ByteString,
    poolSize :: Int,
    allowedOrigins :: [ByteString]
  }
  deriving (Show)

-- | from PORT, DATABASE_URL, DB_POOL_SIZE, CORS_ORIGINS (comma-separated)
loadConfig :: IO Config
loadConfig = do
  p <- readEnv "PORT" 8080
  url <- fromMaybe "host=localhost port=5432 dbname=hit user=postgres password=postgres" <$> lookupEnv "DATABASE_URL"
  n <- readEnv "DB_POOL_SIZE" 4
  origins <- maybe [] splitOrigins <$> lookupEnv "CORS_ORIGINS"
  pure (Config p (BS.pack url) n origins)

readEnv :: (Read a) => String -> a -> IO a
readEnv name def = do
  m <- lookupEnv name
  case m of
    Nothing -> pure def
    Just s -> maybe (fail ("invalid " <> name <> ": " <> s)) pure (readMaybe s)

splitOrigins :: String -> [ByteString]
splitOrigins = filter (not . BS.null) . map BS.strip . BS.split ',' . BS.pack
