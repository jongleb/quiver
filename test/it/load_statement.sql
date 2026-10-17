-- Prints the LOAD DATA statement for table @t of quiver_it in dialect @dialect
-- ('rfc4180' or 'mysql-load-data'), mapping hex bytes and BIT values back.
SELECT CONCAT(
  'LOAD DATA LOCAL INFILE ''/dev/stdin'' INTO TABLE quiver_it.`', @t, '` CHARACTER SET utf8mb4 ',
  IF(@dialect = 'rfc4180',
     'FIELDS TERMINATED BY '','' OPTIONALLY ENCLOSED BY ''"'' ESCAPED BY '''' LINES TERMINATED BY ''\\r\\n'' IGNORE 1 LINES',
     'FIELDS TERMINATED BY '','' ENCLOSED BY ''"'' ESCAPED BY ''\\\\'' LINES TERMINATED BY ''\\n'''),
  ' (', GROUP_CONCAT(CONCAT('@c', ORDINAL_POSITION) ORDER BY ORDINAL_POSITION), ') SET ',
  GROUP_CONCAT(
    CONCAT('`', COLUMN_NAME, '` = ',
      CASE
        WHEN DATA_TYPE IN ('binary', 'varbinary', 'tinyblob', 'blob', 'mediumblob', 'longblob')
          THEN CONCAT('UNHEX(', IF(@dialect = 'rfc4180', CONCAT('NULLIF(@c', ORDINAL_POSITION, ', ''\\\\N'')'), CONCAT('@c', ORDINAL_POSITION)), ')')
        WHEN DATA_TYPE = 'bit'
          THEN CONCAT('CAST(', IF(@dialect = 'rfc4180', CONCAT('NULLIF(@c', ORDINAL_POSITION, ', ''\\\\N'')'), CONCAT('@c', ORDINAL_POSITION)), ' AS UNSIGNED)')
        ELSE IF(@dialect = 'rfc4180', CONCAT('NULLIF(@c', ORDINAL_POSITION, ', ''\\\\N'')'), CONCAT('@c', ORDINAL_POSITION))
      END)
    ORDER BY ORDINAL_POSITION))
FROM information_schema.COLUMNS
WHERE TABLE_SCHEMA = 'quiver_it' AND TABLE_NAME = @t
  AND EXTRA NOT LIKE '%VIRTUAL GENERATED%'
  AND EXTRA NOT LIKE '%STORED GENERATED%'
  AND EXTRA NOT LIKE '%PERSISTENT GENERATED%';
