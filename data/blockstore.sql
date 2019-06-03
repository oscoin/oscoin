------------------------------------
-- SQLite3 Schema for Block Store --
------------------------------------

CREATE TABLE IF NOT EXISTS "blocks" (
  "hash"          char(64)      UNIQUE NOT NULL PRIMARY KEY,
  "height"        integer       UNIQUE NOT NULL,
  "parenthash"    char(64)      UNIQUE,
  "datahash"      char(64)      NOT NULL,
  "statehash"     char(64)      NOT NULL,
  "timestamp"     integer       NOT NULL UNIQUE,
  "difficulty"    integer       NOT NULL,
  "seal"          blob          NOT NULL,

  "beneficiary"   blob          NOT NULL,

  FOREIGN KEY ("parenthash")
    REFERENCES blocks ("hash")
);

CREATE UNIQUE INDEX IF NOT EXISTS block_height_ix ON blocks(height);

CREATE TABLE IF NOT EXISTS "transactions" (
  -- Tx
  "hash"       char(64)      NOT NULL PRIMARY KEY,
  "network"    int           NOT NULL,
  "signature"  blob          NOT NULL,

  -- TxPayload
  "messages"   blob          NOT NULL,
  "nonce"      integer       NOT NULL,
  "fee"        integer       NOT NULL,
  "burn"       integer       NOT NULL,
  "author"     char(64)      NOT NULL,

  -- Foreign key
  "blockhash"  char(64)      NOT NULL,

  FOREIGN KEY ("blockhash")
    REFERENCES blocks ("hash")
    ON DELETE CASCADE
);
