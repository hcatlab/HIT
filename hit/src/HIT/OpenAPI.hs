{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | openapi document derived from 'API.HITAPI'
module HIT.OpenAPI
  ( hitOpenAPI,
  )
where

import Control.Lens ((&), (.~), (<>~), (?~))
import Data.Aeson (camelTo2, toJSON)
import Data.Kind (Type)
import Data.OpenApi
import Data.Proxy (Proxy (..))
import Data.Time (DayOfWeek (..))
import Data.Typeable (Typeable)
import GHC.Exts (fromList)
import HIT.API qualified as API
import HIT.Types qualified as Types
import Refined.Unsafe.Type (Refined (..))
import Servant (AuthProtect, (:>))
import Servant.OpenApi (HasOpenApi (..))

hitOpenAPI :: OpenApi
hitOpenAPI =
  toOpenApi (Proxy :: Proxy API.HITAPI)
    & info . title .~ "HIT API"
    & info . version .~ "0.1.0"
    & info . description ?~ "Goals, habits and intentions. Protected endpoints take a bearer token from /signup or /login."
    & servers .~ ["/api"]

-- | bearer token on every route below
instance (HasOpenApi api) => HasOpenApi (AuthProtect "api-token" :> api) where
  toOpenApi _ =
    toOpenApi (Proxy :: Proxy api)
      & components . securitySchemes <>~ SecurityDefinitions (fromList [("bearer", SecurityScheme (SecuritySchemeHttp (HttpSchemeBearer Nothing)) Nothing)])
      & allOperations . security .~ [SecurityRequirement (fromList [("bearer", [])])]

deriving newtype instance (Typeable entity) => ToSchema (Types.Identifier entity)

deriving newtype instance ToParamSchema (Types.Identifier entity)

deriving newtype instance ToSchema Types.Username

-- | a refined value has the schema of its underlying type
deriving via (a :: Type) instance (ToSchema a, Typeable (predicate :: Type)) => ToSchema (Refined predicate a)

deriving newtype instance ToParamSchema Types.Username

instance ToSchema Types.Interval where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions {constructorTagModifier = camelTo2 '_'}

instance ToSchema Types.Weekdays where
  declareNamedSchema _ =
    pure
      $ NamedSchema (Just "Weekdays")
      $ mempty
        & type_ ?~ OpenApiArray
        & items ?~ OpenApiItemsObject (Inline (mempty & type_ ?~ OpenApiString & enum_ ?~ map toJSON [Monday .. Sunday]))

instance ToSchema Types.APIToken where
  declareNamedSchema = genericDeclareNamedSchema defaultSchemaOptions {unwrapUnaryRecords = True}

instance ToSchema API.PublicUser

instance ToSchema API.HealthResponse

instance ToSchema API.SignupRequest

instance ToSchema API.SignupResponse

instance ToSchema API.LoginRequest

instance ToSchema API.LoginResponse

instance ToSchema API.CreateGoalRequest

instance ToSchema API.UpdateGoalRequest

instance ToSchema API.GoalResponse

instance ToSchema API.CreateUserRequest

instance ToSchema API.UpdateUserRequest

instance ToSchema API.UserResponse

instance ToSchema API.HabitRequest

instance ToSchema API.HabitResponse

instance ToSchema API.CreateIntentionRequest

instance ToSchema API.UpdateIntentionRequest

instance ToSchema API.IntentionResponse
