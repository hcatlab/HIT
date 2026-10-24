{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module HIT.CRUD
  ( CRUDAPI,
    CRUDResource (..),
    CRUDAPIFor,
    crudServer,
  )
where

import Control.Monad ((>=>))
import GHC.TypeLits (Symbol)
import HIT.Env (AppM)
import HIT.Types.Identifier (Entity, EntityKey)
import HIT.Types.User (User)
import Servant
import Prelude hiding (id, read)

-- | a crud api is the user-scoped list, create, read, update and delete routes of a resource
type CRUDAPI (label :: Symbol) key response create update =
  Get '[JSON] [response]
    :<|> ReqBody '[JSON] create :> Post '[JSON] response
    :<|> Capture label key :> Get '[JSON] response
    :<|> Capture label key :> ReqBody '[JSON] update :> Put '[JSON] response
    :<|> Capture label key :> Delete '[JSON] NoContent

-- | a crud resource is an entity with its request and response types, methods applied as @list \@entity@
class CRUDResource (entity :: Entity) where
  -- | name of the captured key in routes
  type Label entity :: Symbol

  -- | body of create
  type CreateRequest entity

  -- | body of update
  type UpdateRequest entity

  -- | body of every response
  type Response entity

  -- | all of the user's entities
  list :: User -> AppM [Response entity]

  -- | a new entity
  create :: User -> CreateRequest entity -> AppM (Response entity)

  -- | the entity, nothing if missing or not the user's
  read :: User -> EntityKey entity -> AppM (Maybe (Response entity))

  -- | the updated entity, nothing if missing or not the user's
  update :: User -> EntityKey entity -> UpdateRequest entity -> AppM (Maybe (Response entity))

  -- | whether the entity existed and was deleted
  delete :: User -> EntityKey entity -> AppM Bool

-- | 'CRUDAPI' of a 'CRUDResource'
type CRUDAPIFor entity = CRUDAPI (Label entity) (EntityKey entity) (Response entity) (CreateRequest entity) (UpdateRequest entity)

-- | server of a 'CRUDResource' for the authenticated user, 404 when read, update or delete find nothing
crudServer :: forall entity. (CRUDResource entity) => User -> ServerT (CRUDAPIFor entity) AppM
crudServer user =
  list @entity user
    :<|> create @entity user
    :<|> (read @entity user >=> found)
    :<|> (\key body -> update @entity user key body >>= found)
    :<|> (delete @entity user >=> \deleted -> if deleted then pure NoContent else throwError err404)
  where
    found :: Maybe a -> AppM a
    found = maybe (throwError err404) pure
