{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeOperators #-}

module HIT.API
  ( HITAPI,
    HITAPIWithDocs,
    module HIT.API.Auth,
    module HIT.API.Goals,
    module HIT.API.Health,
    module HIT.API.Users,
    module HIT.API.Habits,
    module HIT.API.Intentions,
  )
where

import Data.OpenApi (OpenApi)
import HIT.API.Auth
import HIT.API.Goals
import HIT.API.Habits
import HIT.API.Health
import HIT.API.Intentions
import HIT.API.Users
import Servant

-- | hit backend api
type HITAPI =
  "health" :> Get '[JSON] HealthResponse
    :<|> "signup" :> ReqBody '[JSON] SignupRequest :> Post '[JSON] SignupResponse
    :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
    :<|> AuthProtect "api-token" :> "me" :> Get '[JSON] PublicUser
    :<|> AuthProtect "api-token" :> "goals" :> GoalsAPI
    :<|> AuthProtect "api-token" :> "users" :> UsersAPI
    :<|> AuthProtect "api-token" :> "habits" :> HabitsAPI
    :<|> AuthProtect "api-token" :> "intentions" :> IntentionsAPI

-- | 'HITAPI' plus openapi.json
type HITAPIWithDocs = HITAPI :<|> "openapi.json" :> Get '[JSON] OpenApi
