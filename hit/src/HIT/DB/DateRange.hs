{-# LANGUAGE OverloadedStrings #-}

-- | postgres daterange columns of inclusive first and last days
module HIT.DB.DateRange
  ( DateRange,
    dateRange,
    rangeDays,
    dateRangeType,
  )
where

import Data.Time (Day, addDays)
import Database.Beam (DataType (..))
import Database.Beam.Postgres (PgBoundType (..), PgDateRange, PgRange (..), PgRangeBound (..), Postgres, exclusive, inclusive, unbounded)
import Database.Beam.Postgres.Syntax (PgDataTypeDescr (..), PgDataTypeSyntax (..), emit, pgDataTypeJSON)
import Database.PostgreSQL.Simple.TypeInfo.Static (daterange, typoid)

-- | a date range is a daterange column, stored canonically as [first, last + 1)
type DateRange = PgRange PgDateRange Day

-- | the days from first to last, unbounded without a last
dateRange :: Day -> Maybe Day -> DateRange
dateRange first final = PgRange (inclusive first) (maybe unbounded (exclusive . addDays 1) final)

-- | first and last day, nothing for an empty range or one without a first day
rangeDays :: DateRange -> Maybe (Day, Maybe Day)
rangeDays (PgRange (PgRangeBound lowerType (Just lower)) (PgRangeBound upperType upper)) =
  Just (firstDay, lastDay <$> upper)
  where
    firstDay = if lowerType == Inclusive then lower else addDays 1 lower
    lastDay u = if upperType == Exclusive then addDays (-1) u else u
rangeDays _ = Nothing

-- | the daterange type in the checked schema, matched by its fixed oid
dateRangeType :: DataType Postgres DateRange
dateRangeType = DataType (PgDataTypeSyntax (PgDataTypeDescrOid (typoid daterange) Nothing) (emit "daterange") (pgDataTypeJSON "daterange"))
