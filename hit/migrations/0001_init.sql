-- baseline
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";

DO $$
BEGIN
  IF NOT EXISTS (SELECT 1 FROM pg_type WHERE typname = 'interval_kind') THEN
    CREATE TYPE interval_kind AS ENUM ('daily', 'weekly');
  END IF;
END;
$$;

CREATE TABLE IF NOT EXISTS users (
  id TEXT PRIMARY KEY,
  email TEXT UNIQUE NOT NULL,
  password_hash TEXT NOT NULL,
  api_token TEXT UNIQUE NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CHECK (modified_at >= created_at)
);

CREATE TABLE IF NOT EXISTS goals (
  id UUID PRIMARY KEY,
  "user" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  description TEXT NOT NULL,
  number INTEGER NOT NULL,
  color TEXT NOT NULL,
  start_date DATE NOT NULL,
  end_date DATE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CHECK (end_date IS NULL OR end_date > start_date),
  CHECK (length(trim(description)) > 0),
  CHECK (modified_at >= created_at),
  UNIQUE ("user", number)
);

CREATE TABLE IF NOT EXISTS habits (
  id UUID PRIMARY KEY,
  "user" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  description TEXT,
  interval TEXT NOT NULL,
  sort JSON NOT NULL,
  rate JSON NOT NULL,
  deadline JSON NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CHECK (modified_at >= created_at)
);

CREATE TABLE IF NOT EXISTS goal_habits (
  habit_id UUID NOT NULL REFERENCES habits(id) ON DELETE CASCADE,
  goal_id UUID NOT NULL REFERENCES goals(id) ON DELETE CASCADE,
  PRIMARY KEY (habit_id, goal_id)
);

CREATE TABLE IF NOT EXISTS intentions (
  id UUID PRIMARY KEY,
  "user" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  description TEXT,
  interval TEXT NOT NULL,
  rate JSON NOT NULL,
  deadline JSON NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CHECK (modified_at >= created_at)
);

CREATE TABLE IF NOT EXISTS goal_intentions (
  intention_id UUID NOT NULL REFERENCES intentions(id) ON DELETE CASCADE,
  goal_id UUID NOT NULL REFERENCES goals(id) ON DELETE CASCADE,
  PRIMARY KEY (intention_id, goal_id)
);
