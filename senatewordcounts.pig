SET default_parallel 100;

SET mapreduce.map.memory.mb 8192;
SET mapred.max.map.failures.percent 10;
--SET mapreduce.reduce.memory.mb 4096;
--SET mapreduce.reduce.java.opts -Xmx3686m;

set mapreduce.reduce.memory.mb 16000;
set mapreduce.reduce.java.opts -Xmx8196m

--SET mapreduce.reduce.memory.mb 8196;
--SET mapreduce.reduce.java.opts -Xmx4098m;

REGISTER lib/ia-porky-jar-with-dependencies.jar;

REGISTER 'extrawordsEKGJonJohn.py' USING jython AS myfuncs;
--REGISTER '8Sept2017EkgJonJohn.py' USING jython as myfuncs;

DEFINE FROMJSON org.archive.porky.FromJSON();
DEFINE SequenceFileLoader org.archive.porky.SequenceFileLoader();
DEFINE SURTURL org.archive.porky.SurtUrlKey();

Archive = LOAD '$I_PARSED_DATA' USING SequenceFileLoader() AS (key:chararray, value:chararray);

Archive = FOREACH Archive GENERATE FROMJSON(value) AS m:[];

Archive = FILTER Archive BY m#'errorMessage' is null;
ExtractedCounts = FOREACH Archive GENERATE myfuncs.pickURLs(m#'url'),
    m#'url' AS src:chararray,
    SURTURL(m#'url') as surt:chararray,
    REPLACE(m#'digest','sha1:','') AS checksum:chararray,
    m#'date' as date:chararray,
    myfuncs.Threat_countWords(m#'boiled');

ExtractedCounts = FILTER ExtractedCounts BY (date MATCHES '.*200612.*' OR date MATCHES '.*200611.*'
    OR date MATCHES '.*200812.*' OR date MATCHES '.*200811.*' OR date MATCHES '.*201011.*'
    OR date MATCHES '.*201012.*' OR date MATCHES '.*201212.*'  OR date MATCHES '.*201211.*');

Checksum = LOAD '$I_CHECKSUM_DATA' USING PigStorage() AS (surt:chararray, date:chararray, checksum:chararray);

CountsJoinChecksum = JOIN ExtractedCounts BY (surt, checksum), Checksum BY (surt, checksum);

FullCounts = FOREACH CountsJoinChecksum GENERATE
    ExtractedCounts::src as src,
    Checksum::date as date,
    ExtractedCounts::counts as counts,
    ExtractedCounts::URLs as URLs;

GroupedCounts = GROUP FullCounts BY URLs;

GroupedCounts = FOREACH GroupedCounts GENERATE
    group AS src,
--    group AS URLs,
   FLATTEN(FullCounts); 
--AS (date:int, counts:bag{:tuple(word:chararray,count:int)}, URLs:chararray);
--    FLATTEN(myfuncs.fillInCounts(FullCounts)) AS (year:int, month:int, word:chararray, count:int, filled:int, afterLast:int, URLs:chararray);

--GroupedCounts = FOREACH GroupedCounts GENERATE
--    year AS SUBSTRING(date, 0,3);

--GroupedCounts = FOREACH GroupedCounts GENERATE
--    month AS SUBSTRING(date, 4,5);

GroupedCounts = FOREACH GroupedCounts GENERATE
    src AS src,
    date AS date,
    SUBSTRING(date, 0,4),
    SUBSTRING(date, 4,6),
    URLs AS URLs,
    FLATTEN(counts);

--GroupedCounts2 = FOREACH GroupedCounts GENERATE
--    date AS date, word AS word, count AS count, URLs AS URLs;

--GroupedCounts2 = FOREACH GroupedCounts GENERATE
--    year AS year, month AS month, word AS word, count AS count, URLs AS URLs;

--STORE GroupedCounts2 INTO '$O_DATA_DIR';
STORE GroupedCounts INTO '$O_DATA_DIR';
