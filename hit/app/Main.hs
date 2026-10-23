module Main where

import Data.Pool (withResource)
import HIT.App (mkApp)
import HIT.Config (Config (..), loadConfig)
import HIT.DB (migrate)
import HIT.Env (Env (..), newEnv)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  cfg <- loadConfig
  env <- newEnv cfg
  withResource (pool env) migrate
  putStrLn ("Starting HIT server on port " <> show (port cfg) <> "...")
  run (port cfg) (mkApp env)
