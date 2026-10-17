DROP DATABASE IF EXISTS quiver_bank;
CREATE DATABASE quiver_bank;
USE quiver_bank;
SET SESSION cte_max_recursion_depth = 100000;

CREATE TABLE accounts (
  id INT NOT NULL PRIMARY KEY,
  balance BIGINT NOT NULL,
  version BIGINT NOT NULL
);
INSERT INTO accounts
  WITH RECURSIVE seq (n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM seq WHERE n < 20000)
  SELECT n, 1000, 0 FROM seq;

CREATE TABLE ledger (
  id BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  debit INT NOT NULL,
  credit INT NOT NULL
);

CREATE TABLE churn (running INT NOT NULL);
INSERT INTO churn VALUES (1);

DELIMITER //
CREATE PROCEDURE transfer_until_stopped()
BEGIN
  DECLARE a INT;
  DECLARE b INT;
  WHILE (SELECT running FROM churn) = 1 DO
    SET a = 1 + FLOOR(RAND() * 20000);
    SET b = 1 + FLOOR(RAND() * 20000);
    START TRANSACTION;
    UPDATE accounts SET balance = balance - 1, version = version + 1 WHERE id = a;
    UPDATE accounts SET balance = balance + 1, version = version + 1 WHERE id = b;
    INSERT INTO ledger (debit, credit) VALUES (a, b);
    COMMIT;
  END WHILE;
END //
DELIMITER ;
