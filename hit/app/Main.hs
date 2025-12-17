{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Lib
import Network.Wai ()
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant

main :: IO ()
main = do
  putStrLn "Starting HIT server on port 8080..."
  run 8080 app

app :: Application
app = simpleCors $ serve (Proxy :: Proxy HITApi) server

server :: Server HITApi
server = pure (HealthResponse "ok")
