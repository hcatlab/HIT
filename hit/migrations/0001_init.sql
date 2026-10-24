CREATE EXTENSION btree_gist;

CREATE TYPE interval_kind AS ENUM ('daily', 'weekly');

CREATE TABLE users (
  id TEXT PRIMARY KEY,
  email TEXT NOT NULL,
  password_hash TEXT NOT NULL,
  api_token TEXT UNIQUE NOT NULL,
  timezone TEXT NOT NULL DEFAULT 'UTC',
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CHECK (modified_at >= created_at)
);

CREATE UNIQUE INDEX users_email_lower ON users (lower(email));

CREATE TABLE goals (
  id UUID PRIMARY KEY DEFAULT uuidv7(),
  "user" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  name TEXT NOT NULL,
  description TEXT NOT NULL,
  number INTEGER NOT NULL,
  color TEXT NOT NULL,
  active DATERANGE NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CHECK (NOT isempty(active) AND NOT lower_inf(active)),
  CHECK (upper_inf(active) OR upper(active) - lower(active) >= 2),
  CHECK (length(trim(description)) > 0),
  CHECK (modified_at >= created_at),
  UNIQUE ("user", number)
);

CREATE TABLE habits (
  id UUID PRIMARY KEY DEFAULT uuidv7(),
  "user" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  title TEXT NOT NULL CHECK (length(trim(title)) > 0),
  target DOUBLE PRECISION NOT NULL DEFAULT 1 CHECK (target > 0),
  unit TEXT,
  "interval" interval_kind NOT NULL,
  -- iso weekday bitmask, bit 0 is monday, present iff daily
  weekdays SMALLINT CHECK (weekdays BETWEEN 1 AND 127),
  due_time TIME,
  active DATERANGE NOT NULL,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  CHECK (("interval" = 'daily') = (weekdays IS NOT NULL)),
  CHECK (NOT isempty(active) AND NOT lower_inf(active)),
  CHECK (modified_at >= created_at)
);

CREATE TABLE habit_goals (
  habit_id UUID NOT NULL REFERENCES habits(id) ON DELETE CASCADE,
  goal_id UUID NOT NULL REFERENCES goals(id) ON DELETE NO ACTION,
  PRIMARY KEY (habit_id, goal_id)
);

CREATE TABLE intentions (
  id UUID PRIMARY KEY DEFAULT uuidv7(),
  "user" TEXT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  habit_id UUID REFERENCES habits(id) ON DELETE SET NULL,
  title TEXT NOT NULL CHECK (length(trim(title)) > 0),
  target DOUBLE PRECISION NOT NULL DEFAULT 1 CHECK (target > 0),
  unit TEXT,
  progress DOUBLE PRECISION NOT NULL DEFAULT 0 CHECK (progress >= 0),
  "interval" interval_kind NOT NULL,
  "window" DATERANGE NOT NULL,
  due_time TIME,
  done_at TIMESTAMP WITH TIME ZONE,
  created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  modified_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
  -- a daily window is one day, a weekly one a monday-to-sunday iso week
  CHECK (
    ("interval" = 'daily' AND upper("window") - lower("window") = 1)
    OR ("interval" = 'weekly' AND upper("window") - lower("window") = 7 AND extract(isodow FROM lower("window")) = 1)
  ),
  CHECK ((done_at IS NOT NULL) = (progress >= target)),
  CHECK (modified_at >= created_at),
  EXCLUDE USING gist (habit_id WITH =, "window" WITH &&)
);

CREATE INDEX intentions_user_window ON intentions USING gist ("user", "window");

CREATE TABLE intention_goals (
  intention_id UUID NOT NULL REFERENCES intentions(id) ON DELETE CASCADE,
  goal_id UUID NOT NULL REFERENCES goals(id) ON DELETE NO ACTION,
  PRIMARY KEY (intention_id, goal_id)
);
