{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module HIT.DB.Schema where

import Database.Beam
import HIT.Types.Goal (GoalT (..))
import HIT.Types.User (PrimaryKey (UserId), UserT (..))

data HITDb f = HITDb
  { users :: f (TableEntity UserT),
    goals :: f (TableEntity GoalT)
  }
  deriving (Generic)

instance Database be HITDb

hitDb :: DatabaseSettings be HITDb
hitDb =
  defaultDbSettings
    `withDbModification` dbModification
      { users =
          modifyTableFields
            ( User
                (fieldNamed "id")
                (fieldNamed "email")
                (fieldNamed "password_hash")
                (fieldNamed "api_token")
            ),
        goals =
          modifyTableFields
            ( Goal
                (fieldNamed "id")
                (UserId (fieldNamed "user"))
                (fieldNamed "name")
                (fieldNamed "description")
            )
      }
