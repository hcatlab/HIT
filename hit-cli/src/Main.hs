module Main where

import Data.Aeson (FromJSON (..), (.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Client
  ( defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
    responseBody,
  )
import Options.Applicative

data Command = Health deriving (Show)

commandParser :: Parser Command
commandParser =
  hsubparser
    (command "health" (info (pure Health) (progDesc "Check server health")))

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

handleCommand :: Command -> IO ()
handleCommand Health = checkHealth

data HealthResponse = HealthResponse
  { status :: String
  }

instance FromJSON HealthResponse where
  parseJSON = Aeson.withObject "HealthResponse" $ \o ->
    HealthResponse <$> o .: Key.fromString "status"

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
