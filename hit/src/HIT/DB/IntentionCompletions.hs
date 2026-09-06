{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module HIT.DB.IntentionCompletions
  ( markComplete,
    unmarkComplete,
    listTodayCompletions,
  )
where

import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple qualified as PG
import Database.PostgreSQL.Simple.SqlQQ (sql)

-- | Mark an intention as completed for today (idempotent). Only succeeds if
-- the intention belongs to the given user.
markComplete :: Connection -> Text -> UUID -> IO ()
markComplete conn uid iid = do
  _ <-
    PG.execute
      conn
      [sql|
        INSERT INTO intention_completions (intention_id, completed_date)
        SELECT ?::uuid, CURRENT_DATE
        WHERE EXISTS (SELECT 1 FROM intentions WHERE id = ?::uuid AND "user" = ?)
        ON CONFLICT (intention_id, completed_date) DO NOTHING
      |]
      (UUID.toText iid, UUID.toText iid, uid)
  pure ()

-- | Unmark an intention's completion for today. Only succeeds if the
-- intention belongs to the given user.
unmarkComplete :: Connection -> Text -> UUID -> IO ()
unmarkComplete conn uid iid = do
  _ <-
    PG.execute
      conn
      [sql|
        DELETE FROM intention_completions
        USING intentions
        WHERE intention_completions.intention_id = intentions.id
          AND intentions."user" = ?
          AND intention_completions.intention_id = ?::uuid
          AND intention_completions.completed_date = CURRENT_DATE
      |]
      (uid, UUID.toText iid)
  pure ()

-- | List the ids of all intentions completed today for a given user.
listTodayCompletions :: Connection -> Text -> IO [UUID]
listTodayCompletions conn uid = do
  rows <-
    PG.query
      conn
      [sql|
        SELECT ic.intention_id::text
        FROM intention_completions ic
        JOIN intentions i ON i.id = ic.intention_id
        WHERE i."user" = ? AND ic.completed_date = CURRENT_DATE
      |]
      (PG.Only uid) ::
      IO [PG.Only Text]
  pure (mapMaybe (UUID.fromText . PG.fromOnly) rows)
