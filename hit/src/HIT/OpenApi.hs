{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HIT.OpenApi
  ( hitSwagger,
    SwaggerApi,
  )
where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (ToJSON (toJSON))
import Data.HashMap.Strict.InsOrd qualified as IOHM
import Data.Swagger
import Data.Swagger qualified as S
import Data.Text (Text)
import HIT.Api
import HIT.Types
import Servant
import Servant.Swagger

type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

hitSwagger :: Swagger
hitSwagger =
  toSwagger unprotectedApi
    & info . title .~ "HIT API"
    & info . version .~ "0.1.0"
    & info . S.description ?~ "API for Habits, Intentions, and Time tracking. Protected endpoints require Bearer token authentication."
    & host ?~ "localhost:8080"
    & schemes ?~ [Http]
  where
    -- Simplified API type for Swagger generation (without AuthProtect)
    unprotectedApi :: Proxy UnprotectedHITApi
    unprotectedApi = Proxy

type UnprotectedHITApi =
  "health" :> Get '[JSON] HealthResponse
    :<|> "signup" :> ReqBody '[JSON] SignupRequest :> Post '[JSON] SignupResponse
    :<|> "login" :> ReqBody '[JSON] LoginRequest :> Post '[JSON] LoginResponse
    :<|> Header' '[Required] "Authorization" Text :> "me" :> Get '[JSON] PublicUser
    :<|> Header' '[Required] "Authorization" Text :> "goals" :> GoalsApi
    :<|> Header' '[Required] "Authorization" Text :> "users" :> UsersApi
    :<|> Header' '[Required] "Authorization" Text :> "habits" :> HabitsApi
    :<|> Header' '[Required] "Authorization" Text :> "intentions" :> IntentionsApi

-- ToSchema instances for all types

-- Interval
instance ToSchema Interval where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Interval") $
        mempty
          & type_ ?~ SwaggerString
          & enum_ ?~ ["daily", "weekly"]
          & S.description ?~ "Time interval: daily or weekly"

instance ToParamSchema Interval where
  toParamSchema _ =
    mempty
      & type_ ?~ SwaggerString
      & enum_ ?~ ["daily", "weekly"]

-- Sort
instance ToSchema Sort where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Sort") $
        mempty
          & type_ ?~ SwaggerObject
          & S.description ?~ "Sorting order: either yes/no or times count"
          & example ?~ toJSON (YesNo True)

-- Fraction
instance ToSchema Fraction where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Fraction") $
        mempty
          & type_ ?~ SwaggerString
          & S.pattern ?~ "^[0-9]+/[0-9]+$"
          & S.description ?~ "Fraction represented as 'numerator/denominator'"
          & example ?~ toJSON (Fraction 3 4)

-- State
instance ToSchema State where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "State") $
        mempty
          & type_ ?~ SwaggerString
          & enum_ ?~ ["active", "inactive", "deleted"]
          & S.description ?~ "Entity state"

-- Hours
instance ToSchema Hours where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Hours") $
        mempty
          & type_ ?~ SwaggerArray
          & items ?~ SwaggerItemsObject (Inline $ mempty & type_ ?~ SwaggerInteger)
          & S.description ?~ "List of hours (0-23)"
          & example ?~ toJSON (Hours [9, 12, 15])

-- Weekdays
instance ToSchema Weekdays where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "Weekdays") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("monday", Inline $ toSchema (Proxy :: Proxy Hours)),
                ("tuesday", Inline $ toSchema (Proxy :: Proxy Hours)),
                ("wednesday", Inline $ toSchema (Proxy :: Proxy Hours)),
                ("thursday", Inline $ toSchema (Proxy :: Proxy Hours)),
                ("friday", Inline $ toSchema (Proxy :: Proxy Hours)),
                ("saturday", Inline $ toSchema (Proxy :: Proxy Hours)),
                ("sunday", Inline $ toSchema (Proxy :: Proxy Hours))
              ]
          & required .~ ["monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday"]
          & S.description ?~ "Hours for each day of the week"

-- Deadline instances (phantom-typed)
instance ToSchema (Deadline 'Daily) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "DeadlineDaily") $
        toSchema (Proxy :: Proxy Hours)
          & S.description ?~ "Daily deadline (hours)"

instance ToSchema (Deadline 'Weekly) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "DeadlineWeekly") $
        toSchema (Proxy :: Proxy Weekdays)
          & S.description ?~ "Weekly deadline (weekdays with hours)"

-- User types
instance ToSchema ApiToken where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "ApiToken") $
        mempty
          & type_ ?~ SwaggerString
          & S.description ?~ "API authentication token"

instance ToSchema PublicUser where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "PublicUser") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("email", Inline $ mempty & type_ ?~ SwaggerString)
              ]
          & required .~ ["id", "email"]
          & S.description ?~ "Public user information"

-- Auth API types
instance ToSchema SignupRequest where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "SignupRequest") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("email", Inline $ mempty & type_ ?~ SwaggerString),
                ("password", Inline $ mempty & type_ ?~ SwaggerString)
              ]
          & required .~ ["id", "email", "password"]
          & S.description ?~ "Signup request with user credentials"

instance ToSchema SignupResponse where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "SignupResponse") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("token", Inline $ toSchema (Proxy :: Proxy ApiToken)),
                ("user", Inline $ toSchema (Proxy :: Proxy PublicUser))
              ]
          & required .~ ["token", "user"]
          & S.description ?~ "Signup response with auth token and user info"

instance ToSchema LoginRequest where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "LoginRequest") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("password", Inline $ mempty & type_ ?~ SwaggerString)
              ]
          & required .~ ["id", "password"]
          & S.description ?~ "Login request with credentials"

instance ToSchema LoginResponse where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "LoginResponse") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("token", Inline $ toSchema (Proxy :: Proxy ApiToken)),
                ("user", Inline $ toSchema (Proxy :: Proxy PublicUser))
              ]
          & required .~ ["token", "user"]
          & S.description ?~ "Login response with auth token and user info"

-- Health API
instance ToSchema HealthResponse where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "HealthResponse") $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ IOHM.fromList [("status", Inline $ mempty & type_ ?~ SwaggerString)]
          & required .~ ["status"]
          & S.description ?~ "Health check response"

-- Goals API types
instance ToSchema CreateGoalRequest where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "CreateGoalRequest") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("state", Inline $ toSchema (Proxy :: Proxy State))
              ]
          & required .~ ["name", "state"]
          & S.description ?~ "Request to create a new goal"

instance ToSchema UpdateGoalRequest where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "UpdateGoalRequest") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("state", Inline $ toSchema (Proxy :: Proxy State))
              ]
          & required .~ ["name", "state"]
          & S.description ?~ "Request to update a goal"

instance ToSchema GoalResponse where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "GoalResponse") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("state", Inline $ toSchema (Proxy :: Proxy State)),
                ("createdAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time"),
                ("modifiedAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time")
              ]
          & required .~ ["id", "name", "state", "createdAt", "modifiedAt"]
          & S.description ?~ "Goal information"

-- Users API types
instance ToSchema UserResponse where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "UserResponse") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("email", Inline $ mempty & type_ ?~ SwaggerString),
                ("createdAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time"),
                ("modifiedAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time")
              ]
          & required .~ ["id", "email", "createdAt", "modifiedAt"]
          & S.description ?~ "User information"

instance ToSchema CreateUserRequest where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "CreateUserRequest") $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ IOHM.fromList [("id", Inline $ mempty & type_ ?~ SwaggerString), ("email", Inline $ mempty & type_ ?~ SwaggerString), ("password", Inline $ mempty & type_ ?~ SwaggerString)]
          & required .~ ["id", "email", "password"]
          & S.description ?~ "Request to create a user"

instance ToSchema UpdateUserRequest where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "UpdateUserRequest") $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ IOHM.fromList [("email", Inline $ mempty & type_ ?~ SwaggerString)]
          & required .~ ["email"]
          & S.description ?~ "Request to update a user"

-- Habits API types
instance ToSchema HabitDeadline where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "HabitDeadline") $
        mempty
          & type_ ?~ SwaggerObject
          & S.description ?~ "Habit deadline (daily hours or weekly weekdays)"

instance ToSchema HabitView where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "HabitView") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("interval", Inline $ toSchema (Proxy :: Proxy Interval)),
                ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("sort", Inline $ toSchema (Proxy :: Proxy Sort)),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy HabitDeadline)),
                ("createdAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time"),
                ("modifiedAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time")
              ]
          & required .~ ["id", "interval", "name", "sort", "rate", "deadline", "createdAt", "modifiedAt"]
          & S.description ?~ "Habit view with interval and deadline"

instance ToSchema (CreateHabitRequest 'Daily) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "CreateHabitRequestDaily") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("sort", Inline $ toSchema (Proxy :: Proxy Sort)),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Daily)))
              ]
          & required .~ ["name", "sort", "rate", "deadline"]
          & S.description ?~ "Request to create a daily habit"

instance ToSchema (CreateHabitRequest 'Weekly) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "CreateHabitRequestWeekly") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("sort", Inline $ toSchema (Proxy :: Proxy Sort)),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Weekly)))
              ]
          & required .~ ["name", "sort", "rate", "deadline"]
          & S.description ?~ "Request to create a weekly habit"

instance ToSchema (UpdateHabitRequest 'Daily) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "UpdateHabitRequestDaily") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("sort", Inline $ toSchema (Proxy :: Proxy Sort)),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Daily)))
              ]
          & required .~ ["name", "sort", "rate", "deadline"]
          & S.description ?~ "Request to update a daily habit"

instance ToSchema (UpdateHabitRequest 'Weekly) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "UpdateHabitRequestWeekly") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("sort", Inline $ toSchema (Proxy :: Proxy Sort)),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Weekly)))
              ]
          & required .~ ["name", "sort", "rate", "deadline"]
          & S.description ?~ "Request to update a weekly habit"

instance ToSchema (HabitResponse 'Daily) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "HabitResponseDaily") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("sort", Inline $ toSchema (Proxy :: Proxy Sort)),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Daily))),
                ("createdAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time"),
                ("modifiedAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time")
              ]
          & required .~ ["id", "name", "sort", "rate", "deadline", "createdAt", "modifiedAt"]
          & S.description ?~ "Daily habit response"

instance ToSchema (HabitResponse 'Weekly) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "HabitResponseWeekly") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("sort", Inline $ toSchema (Proxy :: Proxy Sort)),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Weekly))),
                ("createdAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time"),
                ("modifiedAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time")
              ]
          & required .~ ["id", "name", "sort", "rate", "deadline", "createdAt", "modifiedAt"]
          & S.description ?~ "Weekly habit response"

-- Intentions API types
instance ToSchema IntentionDeadline where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "IntentionDeadline") $
        mempty
          & type_ ?~ SwaggerObject
          & S.description ?~ "Intention deadline (daily hours or weekly weekdays)"

instance ToSchema IntentionView where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "IntentionView") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("interval", Inline $ toSchema (Proxy :: Proxy Interval)),
                ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy IntentionDeadline)),
                ("createdAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time"),
                ("modifiedAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time")
              ]
          & required .~ ["id", "interval", "name", "rate", "deadline", "createdAt", "modifiedAt"]
          & S.description ?~ "Intention view with interval and deadline"

instance ToSchema (CreateIntentionRequest 'Daily) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "CreateIntentionRequestDaily") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Daily)))
              ]
          & required .~ ["name", "rate", "deadline"]
          & S.description ?~ "Request to create a daily intention"

instance ToSchema (CreateIntentionRequest 'Weekly) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "CreateIntentionRequestWeekly") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Weekly)))
              ]
          & required .~ ["name", "rate", "deadline"]
          & S.description ?~ "Request to create a weekly intention"

instance ToSchema (UpdateIntentionRequest 'Daily) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "UpdateIntentionRequestDaily") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Daily)))
              ]
          & required .~ ["name", "rate", "deadline"]
          & S.description ?~ "Request to update a daily intention"

instance ToSchema (UpdateIntentionRequest 'Weekly) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "UpdateIntentionRequestWeekly") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Weekly)))
              ]
          & required .~ ["name", "rate", "deadline"]
          & S.description ?~ "Request to update a weekly intention"

instance ToSchema (IntentionResponse 'Daily) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "IntentionResponseDaily") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Daily))),
                ("createdAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time"),
                ("modifiedAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time")
              ]
          & required .~ ["id", "name", "rate", "deadline", "createdAt", "modifiedAt"]
          & S.description ?~ "Daily intention response"

instance ToSchema (IntentionResponse 'Weekly) where
  declareNamedSchema _ =
    return $
      NamedSchema (Just "IntentionResponseWeekly") $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ IOHM.fromList
              [ ("id", Inline $ mempty & type_ ?~ SwaggerString),
                ("name", Inline $ mempty & type_ ?~ SwaggerString),
                ("description", Inline $ mempty & type_ ?~ SwaggerString),
                ("rate", Inline $ toSchema (Proxy :: Proxy Fraction)),
                ("deadline", Inline $ toSchema (Proxy :: Proxy (Deadline 'Weekly))),
                ("createdAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time"),
                ("modifiedAt", Inline $ mempty & type_ ?~ SwaggerString & format ?~ "date-time")
              ]
          & required .~ ["id", "name", "rate", "deadline", "createdAt", "modifiedAt"]
          & S.description ?~ "Weekly intention response"
