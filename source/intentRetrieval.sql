/* replace [platform] with yours */
WITH main AS (
  SELECT
    fullVisitorId,
    visitId,
    hits.eventInfo.eventCategory,
    hits.eventInfo.eventAction,
    totals.timeOnsite,
    hits.page.pagePath as pagePath,

    
    visitStartTime,
    CONCAT(fullVisitorId,' ', CAST(visitId as STRING)) as sessionId,
    hits.hitNumber,
    hits.time as hitTime,
    row_number() over(partition by CONCAT(fullVisitorId,' ', CAST(visitId as STRING)) order by hits.time) as rn,
    min(IF(REGEXP_CONTAINS(hits.page.pagePath,'/player/'), hits.time / 1000,  NULL)) OVER sessionTime as timeToFirstPlay,
    min(CASE WHEN REGEXP_CONTAINS(hits.page.pagePath,'[platform].com/trailer/') THEN hits.time / 1000 ELSE NULL END) OVER sessionTime as timeToFirstTrailer,
    ( SELECT AS STRUCT
      MAX(IF(index=37, value, NULL)) as strip_name, 
      MAX(IF(index=43, value, NULL)) as seniority,
      MAX(IF(index=20 AND VALUE NOT IN ("anonymous", "unavailable"), value, NULL)) as Vuser,
      MAX(IF(index=11 AND VALUE != "", value, NULL))  as gender,
      MAX(IF(index=18 AND VALUE != "", value, NULL)) as birthDate
     FROM UNNEST(hits.customDimensions)) as cd

  FROM `vl-bigquery.79934676.ga_sessions_*`, UNNEST(hits) AS hits
  WHERE _TABLE_SUFFIX BETWEEN '20211118' AND '20211118' #'20220120'
  WINDOW sessionTime AS (PARTITION BY CONCAT(fullVisitorId,' ', CAST(visitId as STRING)) ORDER BY hits.time)
  
)

SELECT
  sessionId,
  max(max(cd.Vuser)) OVER (PARTITION BY sessionId) as Vuser,
  MAX(cd.gender) AS gender,
  MAX(cd.birthDate) AS birthDate,
  MIN(cd.seniority) AS seniority,    
#  max(timeOnSite) as sessionLength, counting the hits was more reliable
  max(hitTime) / 1000 as sessionLength,  

  COUNT(cd.strip_name) as nStrips,  
  MIN(visitStartTime) as visitStartTime,
  
  COUNTIF(CONTAINS_SUBSTR(pagePath,'[platform].com/account')) as nAccounts,
  COUNTIF(pagePath = 'www.[platform].com/zoeken' ) as nSearches,
  COUNTIF(eventCategory = "navigation" AND eventAction = "click.profile-menu") as nProfileClicks,
  COUNTIF(eventCategory = "content curation" AND eventAction = "click.add-watchlist") as nBookmarks, # I removed DISTINCT here, because one person might add several bookmarks from the same page
  COUNT(DISTINCT IF(CONTAINS_SUBSTR(pagePath,'/series/'), pagePath, NULL)) as nSeriesDescr,
  COUNT(DISTINCT IF(CONTAINS_SUBSTR(pagePath,'/films/'), pagePath, NULL)) as nMoviesDescr,
  COUNT(DISTINCT IF(CONTAINS_SUBSTR(pagePath,'/player/'), pagePath, NULL)) as numPlays,
  COUNT(DISTINCT IF(CONTAINS_SUBSTR(pagePath,'[platform].com/trailer/'), pagePath, NULL)) as numTrailerPlays,  
  MAX(timeToFirstPlay) as timeToFirstPlay,
  MAX(timeToFirstTrailer) as timeToFirstPlay

From main
GROUP BY sessionId
ORDER BY sessionLength DESC
