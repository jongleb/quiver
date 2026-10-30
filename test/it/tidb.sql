-- TiDB source for the tidb IT step: every type TiDB shares with MySQL, the kinds of
-- key quiver chunks by, regions split so tables read in several parts, and a view.
SET SESSION cte_max_recursion_depth = 100000;
DROP DATABASE IF EXISTS quiver_tidb;
CREATE DATABASE quiver_tidb;
USE quiver_tidb;

CREATE TABLE all_types (
  id BIGINT PRIMARY KEY CLUSTERED,
  i8 TINYINT, u8 TINYINT UNSIGNED, i16 SMALLINT, i24 MEDIUMINT, i32 INT, u64 BIGINT UNSIGNED,
  f FLOAT, d DOUBLE, dec_ DECIMAL(30,10),
  c CHAR(10), vc VARCHAR(200) NOT NULL, tx TEXT, bin_ BINARY(4), vb VARBINARY(40), bl BLOB,
  dt DATE, dtm DATETIME(6), ts TIMESTAMP(3) NULL, tm TIME(2), y YEAR,
  j JSON, e ENUM('red','green',''), s SET('a','b','c'), b BIT(10)
);
INSERT INTO all_types VALUES
  (1, -128, 255, -32768, -8388608, -2147483648, 18446744073709551615,
   -3.4e38, 1.7976931348623157e308, -99999999999999999999.9999999999,
   'abc', 'Mary Johnson', 'line one\nline two\ttab \\ backslash', X'00FF0102', X'', X'DEADBEEF',
   '1000-01-01', '1000-01-01 00:00:00.000000', '1970-01-01 00:00:01.000', '-838:59:59.00', 1901,
   '{"a": [1, 2.5, -0.0, 1e-7], "b": null, "c": "\\u00e9 \\" quote"}', 'red', 'a,c', b'1010101010'),
  (2, 127, 0, 32767, 8388607, 2147483647, 0,
   1.5, -0.0, 99999999999999999999.9999999999,
   '', '', '', X'00000000', X'00', '',
   '9999-12-31', '9999-12-31 23:59:59.999999', '2038-01-19 03:14:07.999', '838:59:59.00', 2155,
   '[]', '', '', b'0'),
  (3, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL,
   NULL, 'ñ ж 中 😀', NULL, NULL, NULL, NULL,
   NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL);

CREATE TABLE big (id BIGINT PRIMARY KEY CLUSTERED, name VARCHAR(40) NOT NULL, amount DECIMAL(12,2), at DATETIME);
INSERT INTO big
  WITH RECURSIVE n (i) AS (SELECT 1 UNION ALL SELECT i + 1 FROM n WHERE i < 50000)
  SELECT i, CONCAT('row ', i), i * 1.25, '2024-01-01' + INTERVAL i MINUTE FROM n;
SPLIT TABLE big BETWEEN (0) AND (50000) REGIONS 8;

CREATE TABLE composite (region VARCHAR(10), code VARCHAR(20), n INT NOT NULL, PRIMARY KEY (region, code) CLUSTERED);
INSERT INTO composite
  WITH RECURSIVE n (i) AS (SELECT 1 UNION ALL SELECT i + 1 FROM n WHERE i < 20000)
  SELECT CONCAT('r', i % 7), CONCAT('c', LPAD(i, 6, '0')), i FROM n;
SPLIT TABLE composite BY ('r1', 'c'), ('r3', 'c'), ('r5', 'c');

CREATE TABLE nonclustered (id INT PRIMARY KEY NONCLUSTERED, parent BIGINT NOT NULL, note VARCHAR(20), KEY (parent));
INSERT INTO nonclustered
  WITH RECURSIVE n (i) AS (SELECT 1 UNION ALL SELECT i + 1 FROM n WHERE i < 10000)
  SELECT i, 10001 - i, IF(i % 3 = 0, NULL, CONCAT('n', i)) FROM n;

CREATE TABLE no_key (msg VARCHAR(30), at TIMESTAMP(6) NULL);
INSERT INTO no_key
  WITH RECURSIVE n (i) AS (SELECT 1 UNION ALL SELECT i + 1 FROM n WHERE i < 10000)
  SELECT CONCAT('log ', i % 5000), '2025-05-05 05:05:05' + INTERVAL i MICROSECOND FROM n;

CREATE TABLE nothing (id INT PRIMARY KEY);

CREATE VIEW recent AS SELECT id, name FROM big WHERE id > 49990;
