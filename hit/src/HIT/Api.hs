{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Api
  ( HITApi,
    module HIT.Api.Auth,
    module HIT.Api.Goals,
    module HIT.Api.Health,
    module HIT.Api.Users,
    module HIT.Api.Habits,
    module HIT.Api.Intentions,
  )
where

import HIT.Api.Auth
import HIT.Api.Goals
import HIT.Api.Habits
import HIT.Api.Health
import HIT.Api.Intentions
import HIT.Api.Users
import HIT.Types.User (PublicUser)
import Servant

-- | API type definition for HIT backend
type HITApi =
  "health" :> Get '[JSON] HealthResponse
    :<|> "signup" :> ReqBody '[JSON] SignupRequest :> Post '[JSON] SignupResponse
    :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
    :<|> AuthProtect "api-token" :> "me" :> Get '[JSON] PublicUser
    :<|> AuthProtect "api-token" :> "goals" :> GoalsApi
    :<|> AuthProtect "api-token" :> "users" :> UsersApi
    :<|> AuthProtect "api-token" :> "habits" :> HabitsApi
    :<|> AuthProtect "api-token" :> "intentions" :> IntentionsApi