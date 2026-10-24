-- | the hit command line client
module Main where

import Data.Aeson (FromJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client
  ( defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
  )
import Options.Applicative

-- | a command is a cli subcommand
data Command
  = -- | check server health
    Health
  deriving (Show)

-- | subcommands
commandParser :: Parser Command
commandParser =
  hsubparser
    (command "health" (info (pure Health) (progDesc "Check server health")))

-- | parse and run a command
main :: IO ()
main = do
  cmd <- execParser opts
  handleCommand cmd
  where
    opts =
      info
        (commandParser <**> helper)
        ( fullDesc
            <> progDesc "HIT CLI - Habit and Intention Tracker command line client"
            <> header "hit - interact with HIT server"
        )

-- | run a command
handleCommand :: Command -> IO ()
handleCommand Health = checkHealth

-- | a health response is the body of the server's health check
newtype HealthResponse = HealthResponse
  { -- | server status
    status :: String
  }
  deriving (FromJSON)

-- | query the local server's health endpoint
checkHealth :: IO ()
checkHealth = do
  putStrLn "Checking server health..."
  manager <- newManager defaultManagerSettings
  req <- parseRequest "http://localhost:8080/health"
  resBody <- responseBody <$> httpLbs req manager
  case (Aeson.eitherDecode' resBody :: Either String HealthResponse) of
    Left err -> do
      putStrLn $ "Failed to decode response: " ++ err
      putStrLn $ "Raw body: " ++ take 500 (show (LBS.toStrict resBody))
    Right hr -> do
      putStrLn $ "Server status: " ++ status hr
