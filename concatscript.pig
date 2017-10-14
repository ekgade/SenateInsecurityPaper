SET default_parallel 20;

SET mapreduce.map.memory.mb 8192;
SET mapred.max.map.failures.percent 10;

REGISTER lib/ia-porky-jar-with-dependencies.jar;

DEFINE FROMJSON org.archive.porky.FromJSON();
DEFINE SequenceFileLoader org.archive.porky.SequenceFileLoader();

WordCounts = LOAD '$I_WORD_COUNTS' AS (url1:chararray, timestamp:int, year:int, month:int, url:chararray, word:chararray, count:int);

GroupedCounts = GROUP WordCounts BY (year, month, url, word);

AggregatedCounts = FOREACH GroupedCounts GENERATE
    group.year AS year, group.month AS month, group.url AS url, group.word AS word,
    SUM(WordCounts.count) as count;

--STORE GroupedCounts INTO '$O_DATA_DIR';
STORE AggregatedCounts INTO '$O_DATA_DIR';
