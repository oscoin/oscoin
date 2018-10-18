------------------------------------
-- SQLite3 Schema for Block Store --
------------------------------------

CREATE TABLE IF NOT EXISTS "blocks" (
  "hash"          char(64)      NOT NULL PRIMARY KEY,
  "parenthash"    char(64)      NOT NULL REFERENCES "blocks",
  "datahash"      char(64)      NOT NULL,
  "timestamp"     integer       NOT NULL,
  "difficulty"    integer       NOT NULL,
  "nonce"         integer       NOT NULL
);

CREATE TABLE IF NOT EXISTS "transactions" (
  "hash"       char(64)      NOT NULL PRIMARY KEY,
  "context"    char(64)      NOT NULL REFERENCES "blocks",
  "message"    text          NOT NULL,
  "author"     char(64)      NOT NULL,
  "chainid"    int           NOT NULL,
  "nonce"      integer       NOT NULL,
  "blockhash"  char(64)      NOT NULL REFERENCES "blocks"
);
