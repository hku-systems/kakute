-- This file is automatically generated by LogicalPlanToSQLSuite.
SELECT key, value, ROUND(AVG(key) OVER (), 2)
FROM parquet_t1 ORDER BY key
--------------------------------------------------------------------------------
SELECT `gen_attr` AS `key`, `gen_attr` AS `value`, `gen_attr` AS `round(avg(key) OVER (  ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING), 2)` FROM (SELECT `gen_attr`, `gen_attr`, round(`gen_attr`, 2) AS `gen_attr` FROM (SELECT gen_subquery_1.`gen_attr`, gen_subquery_1.`gen_attr`, avg(`gen_attr`) OVER (  ROWS BETWEEN UNBOUNDED PRECEDING AND UNBOUNDED FOLLOWING) AS `gen_attr` FROM (SELECT `gen_attr`, `gen_attr` FROM (SELECT `key` AS `gen_attr`, `value` AS `gen_attr` FROM `default`.`parquet_t1`) AS gen_subquery_0) AS gen_subquery_1) AS gen_subquery_2 ORDER BY `gen_attr` ASC) AS parquet_t1