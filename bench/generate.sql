-- Generates the benchmark database at @scale (about 1 GB of table data per
-- unit). Everything but blob payloads is deterministic.
DROP DATABASE IF EXISTS bench;
CREATE DATABASE bench CHARACTER SET utf8mb4;
USE bench;
SET SESSION sql_log_bin = 0;

CREATE TABLE seq (n INT NOT NULL PRIMARY KEY);
INSERT INTO seq WITH RECURSIVE s (n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM s WHERE n < 1000000)
  SELECT n FROM s;

-- Narrow rows, integer key: the per-row overhead path.
CREATE TABLE narrow (
  id BIGINT NOT NULL PRIMARY KEY,
  a INT NOT NULL,
  b BIGINT NOT NULL,
  c DOUBLE NOT NULL,
  d DATETIME NOT NULL,
  KEY by_a (a),
  KEY by_d (d)
);
INSERT INTO narrow
  SELECT (k.n - 1) * 1000000 + s.n, s.n % 100000, s.n * 7919 + k.n, s.n / 7.0,
         TIMESTAMPADD(SECOND, s.n + k.n * 1000, '2020-01-01')
  FROM seq s JOIN seq k ON k.n <= 10 * @scale;

-- Wide rows: text, JSON and decimals, the formatting and escaping path.
CREATE TABLE wide (
  id INT NOT NULL PRIMARY KEY,
  name VARCHAR(64) NOT NULL,
  body TEXT NOT NULL,
  meta JSON NOT NULL,
  price DECIMAL(12, 2) NOT NULL,
  created TIMESTAMP(3) NOT NULL,
  KEY by_name (name),
  KEY by_created (created)
);
INSERT INTO wide
  SELECT (k.n - 1) * 1000000 + s.n,
         CONCAT('name ''', s.n, '" ', MD5(s.n)),
         CONCAT_WS('\n', SHA2(s.n, 256), SHA2(s.n + 1, 512), MD5(k.n), SHA2(s.n * k.n, 512)),
         JSON_OBJECT('n', s.n, 'tags', JSON_ARRAY(MD5(s.n), k.n), 'ok', s.n % 2 = 0),
         (s.n % 1000000) / 100,
         TIMESTAMPADD(MICROSECOND, s.n * 1000, '2021-06-01 00:00:00')
  FROM seq s JOIN seq k ON k.n <= @scale AND s.n <= 800000;

-- String key: the sampled-boundary chunking path.
CREATE TABLE strkey (
  k CHAR(36) NOT NULL PRIMARY KEY,
  v INT NOT NULL,
  note VARCHAR(100) NOT NULL,
  KEY by_v (v)
);
INSERT INTO strkey
  SELECT LEFT(SHA2(CONCAT(s.n, '.', k.n), 256), 36), s.n, REPEAT(CHAR(65 + s.n % 26), 40 + s.n % 60)
  FROM seq s JOIN seq k ON k.n <= @scale;

-- Large values: the bytes-copy path.
CREATE TABLE blobs (id INT NOT NULL PRIMARY KEY, payload LONGBLOB NOT NULL);
INSERT INTO blobs
  SELECT (k.n - 1) * 10000 + s.n, RANDOM_BYTES(1024) FROM seq s JOIN seq k ON k.n <= @scale AND s.n <= 2000;
UPDATE blobs SET payload = REPEAT(CONCAT(payload, RANDOM_BYTES(1024)), 16);

DROP TABLE seq;
ANALYZE TABLE narrow, wide, strkey, blobs;
SELECT TABLE_NAME, TABLE_ROWS, ROUND(DATA_LENGTH / 1024 / 1024) AS mb
  FROM information_schema.TABLES WHERE TABLE_SCHEMA = 'bench';
