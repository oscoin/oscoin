------------------------------------
-- SQLite3 Schema for Block Store --
------------------------------------

CREATE TABLE IF NOT EXISTS "blocks" (
  "hash"          char(64)      UNIQUE NOT NULL PRIMARY KEY,
  "parenthash"    char(64)      UNIQUE,
  "datahash"      char(64)      NOT NULL,
  "statehash"     char(64)      NOT NULL,
  "timestamp"     integer       NOT NULL UNIQUE,
  "difficulty"    integer       NOT NULL,
  "seal"          blob          NOT NULL,

  -- NOTE: The "timestamp" and "parenthash" fields carry an invariant that is
  -- not checked here: the timestamp of a row must always be greater than
  -- the timestamp of the row referenced by the "parenthash" field.

  FOREIGN KEY ("parenthash")
    REFERENCES blocks ("hash")
);

CREATE TABLE IF NOT EXISTS "transactions" (
  "hash"       char(64)      NOT NULL PRIMARY KEY,
  "context"    char(64)      NOT NULL,
  "message"    text          NOT NULL,
  "author"     char(64)      NOT NULL,
  "chainid"    int           NOT NULL,
  "nonce"      integer       NOT NULL,
  "blockhash"  char(64)      NOT NULL,

  FOREIGN KEY ("blockhash")
    REFERENCES blocks ("hash")
    ON DELETE CASCADE
);
