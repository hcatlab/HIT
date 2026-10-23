{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HIT.Crud
  ( CrudApi,
    CrudResource (..),
    CrudApiFor,
    crudServerTc,
  )
where

import Data.Text (Text)
import GHC.TypeLits (Symbol)
import HIT.Env (AppM)
import HIT.Types.User (User)
import Servant
import Prelude hiding (id, read)

-- A generic CRUD API for user-scoped resources, parameterized by a path label
-- and request/response payload types. We capture the resource id as Text.
type CrudApi (label :: Symbol) idCapture resp create update =
  Get '[JSON] [resp]
    :<|> ReqBody '[JSON] create :> Post '[JSON] resp
    :<|> Capture label idCapture :> Get '[JSON] resp
    :<|> Capture label idCapture :> ReqBody '[JSON] update :> Put '[JSON] resp
    :<|> Capture label idCapture :> Delete '[JSON] NoContent

-- Typeclass-based CRUD definition with associated types.
class CrudResource r where
  type Label r :: Symbol
  type InternalId r
  type CreateReq r
  type UpdateReq r
  type Resp r

  parseId :: r -> Text -> Maybe (InternalId r)
  list :: r -> User -> AppM [Resp r]
  create :: r -> User -> CreateReq r -> AppM (Resp r)
  read :: r -> User -> InternalId r -> AppM (Maybe (Resp r))
  update :: r -> User -> InternalId r -> UpdateReq r -> AppM (Maybe (Resp r))
  delete :: r -> User -> InternalId r -> AppM Bool

-- Convenience alias to get the API type from a CrudResource
type CrudApiFor r = CrudApi (Label r) Text (Resp r) (CreateReq r) (UpdateReq r)

-- Build a Server for any CrudResource and authenticated user.
crudServerTc :: forall r. (CrudResource r) => r -> User -> ServerT (CrudApiFor r) AppM
crudServerTc res user = listH :<|> createH :<|> getH :<|> updateH :<|> deleteH
  where
    listH = list res user

    createH = create res user

    getH ridTxt =
      case parseId res ridTxt of
        Nothing -> throwError err400
        Just rid -> do
          m <- read res user rid
          maybe (throwError err404) pure m

    updateH ridTxt req =
      case parseId res ridTxt of
        Nothing -> throwError err400
        Just rid -> do
          m <- update res user rid req
          maybe (throwError err404) pure m

    deleteH ridTxt =
      case parseId res ridTxt of
        Nothing -> throwError err400
        Just rid -> do
          ok <- delete res user rid
          if ok then pure NoContent else throwError err404
