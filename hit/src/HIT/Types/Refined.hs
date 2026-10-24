{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

-- | refinements of primitive values, checked once where they enter
module HIT.Types.Refined
  ( NonBlank,
    Title,
    Description,
    Target,
    Progress,
    GoalNumber,
    defaultTarget,
    noProgress,
    module Refined,
  )
where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Typeable (typeRep)
import Refined

-- | non-blank iff something other than whitespace remains
data NonBlank

instance Predicate NonBlank Text where
  validate predicate t
    | T.null (T.strip t) = throwRefineOtherException (typeRep predicate) "must not be blank"
    | otherwise = Nothing

-- | a title is a non-blank name of a goal, habit or intention
type Title = Refined NonBlank Text

-- | a description is a non-blank explanation of a goal
type Description = Refined NonBlank Text

-- | a target is the positive amount an intention must reach
type Target = Refined Positive Double

-- | progress is the non-negative amount done so far
type Progress = Refined NonNegative Double

-- | a goal number is a goal's positive per-user label
type GoalNumber = Refined Positive Int

-- | a plain check-off, 1
defaultTarget :: Target
defaultTarget = $$(refineTH 1)

-- | nothing done yet, 0
noProgress :: Progress
noProgress = $$(refineTH 0)
