{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module HIT.Handlers.Intentions
  ( intentionsServer,
    intentionResponse,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime)
import Data.Traversable (for)
import HIT.API.Intentions (CreateIntentionRequest (..), IntentionResponse (..), IntentionsAPI, UpdateIntentionRequest (..))
import HIT.CRUD (CRUDResource (..), crudServer)
import HIT.DB (createIntention, deleteIntention, getIntention, listIntentions, updateIntention)
import HIT.Env (AppM, withDb)
import HIT.Handlers.Users (userToday)
import HIT.Types.Identifier (Entity (..), HabitIdentifier, newIdentifier)
import HIT.Types.Intention (Intention (Intention), SomeIntention (..), Source (..), recordProgress)
import HIT.Types.Intention qualified as Intention (Intention (..))
import HIT.Types.Refined (defaultTarget, noProgress)
import HIT.Types.Schedule (Window (..), windowBounds, windowInterval)
import HIT.Types.User (User)
import HIT.Types.User qualified as User (User (..))
import HIT.Validation (validateGoalOwnership)
import Servant
import Prelude hiding (id, read)

instance CRUDResource 'IntentionEntity where
  type Label 'IntentionEntity = "intentionId"
  type CreateRequest 'IntentionEntity = CreateIntentionRequest
  type UpdateRequest 'IntentionEntity = UpdateIntentionRequest
  type Response 'IntentionEntity = IntentionResponse

  list u =
    withDb $ \conn -> map someIntentionResponse <$> listIntentions conn u.id

  -- a one-off for a single day
  create u req = do
    goals <- validateGoalOwnership u.id req.goalIds
    day <- maybe (userToday u) pure req.date
    identifier <- withDb newIdentifier
    now <- liftIO getCurrentTime
    let i = Intention identifier u.id OneOffSource req.title (fromMaybe defaultTarget req.target) req.unit noProgress (DailyWindow day) req.dueTime Nothing goals now now
    withDb $ \conn -> createIntention conn i
    pure (intentionResponse i)

  read u identifier =
    withDb $ \conn -> fmap someIntentionResponse <$> getIntention conn u.id identifier

  update u identifier req = do
    goals <- validateGoalOwnership u.id req.goalIds
    mOld <- withDb $ \conn -> getIntention conn u.id identifier
    for mOld $ \(SomeIntention old) -> do
      now <- liftIO getCurrentTime
      let revised =
            recordProgress
              now
              req.progress
              old
                { Intention.title = req.title,
                  Intention.target = req.target,
                  Intention.unit = req.unit,
                  Intention.dueTime = req.dueTime,
                  Intention.goals = goals,
                  Intention.modifiedAt = now
                }
      withDb $ \conn -> updateIntention conn revised
      pure (intentionResponse revised)

  -- only one-offs, by type
  delete u identifier = do
    mOld <- withDb $ \conn -> getIntention conn u.id identifier
    case mOld of
      Nothing -> pure False
      Just (SomeIntention i) -> case i.source of
        OneOffSource -> True <$ withDb (`deleteIntention` i)
        RecurringSource _ -> throwError err400 {errBody = "habit occurrences can't be deleted, edit the habit instead"}

intentionsServer :: User -> ServerT IntentionsAPI AppM
intentionsServer = crudServer @'IntentionEntity

someIntentionResponse :: SomeIntention -> IntentionResponse
someIntentionResponse (SomeIntention i) = intentionResponse i

intentionResponse :: Intention origin interval -> IntentionResponse
intentionResponse i =
  IntentionResponse
    i.id
    (habitOf i.source)
    i.title
    i.target
    i.unit
    i.progress
    (windowInterval i.window)
    start
    due
    i.dueTime
    i.doneAt
    (NE.toList i.goals)
    i.createdAt
    i.modifiedAt
  where
    (start, due) = windowBounds i.window
    habitOf :: Source origin -> Maybe HabitIdentifier
    habitOf OneOffSource = Nothing
    habitOf (RecurringSource h) = Just h
