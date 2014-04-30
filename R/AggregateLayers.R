##' Aggregate Layers
##' 
##' Aggregate by country, country and year, or some other weight.
##' 
##' 
##' @aliases aggregate_by_country aggregate_by_country_year aggregate_by_country_weighted
##' @export aggregate_by_country aggregate_by_country_year aggregate_by_country_weighted
##' @param df Input data frame.
##' @param col.value Column in data frame containing the value to be
##' aggregated.
##' @param col.country Column in data frame containing the country_id.
##' @param col.weight Column in data frame containing the weight.
##' @return These functions aggregate to region by either country, country and
##' year, or just a weight. These are mostly only used for aggregating a goal's
##' status or trend calculations to region.
##' @keywords layers_navigation
##' @examples
##' 
##'   \dontrun{
##'     aggregate_by_country(df, col.value='value', col.country='country_id')
##'     aggregate_by_country_weighted(df, w, col.value='value', col.country='country_id', col.weight='weight')
##'     aggregate_by_country_year(df, col.value='value', col.country='country_id')
##'   }
##' @name AggregateLayers
aggregate_by_country = function(df, col.value='value', col.country='country_id', lyrs.dat.csv=layers_data.csv){  
  # debug: df = cn; col.value='status'; col.country='country_id'  
  
  library(sqldf)
  
  #rgn_georegions = setNames(rgn_georegions, c('region_id', 'n', 'r0', 'r1', 'r2'))
  load.regions.countries(lyrs.dat.csv) # makes: cntry_georegions
  
  # get data joined to georegions
  q = sprintf("SELECT d.%s AS country_id, c.country_area_km2, d.%s AS value, g.r0, g.r1, g.r2
              FROM df AS d
              JOIN cntry_georegions AS g USING (country_id)
              JOIN cntry AS c USING (country_id)              
              WHERE value IS NOT NULL
              ORDER BY country_id", col.country, col.value)
  d = sqldf(q)
  
  # aggregate into regional area-weighted averages based on actual data
  t_regionals = sqldf(
    "SELECT * FROM (
    -- aggregate into r0 (world) georegion
    SELECT r0, SUM(value * country_area_km2)/SUM(country_area_km2) AS r0_mean, COUNT(*) AS r0_n
    FROM d
    GROUP BY r0
  ) AS t0 JOIN (
    -- aggregate into r1 (continent) georegions
    SELECT r0, r1, SUM(value * country_area_km2)/SUM(country_area_km2) AS r1_mean, COUNT(*) AS r1_n
    FROM d
    GROUP BY r0, r1
  ) AS t1 USING (r0) JOIN (
    -- aggregate into r2 (regional) georegions
    SELECT r0, r1, r2, SUM(value * country_area_km2)/SUM(country_area_km2) AS r2_mean, COUNT(*) AS r2_n
    FROM d
    GROUP BY r0, r1, r2
  ) AS t2 USING (r0, r1)")
  
  # calculate OHI region area weighted average of values using available data
  t_actuals = sqldf("SELECT * FROM  (    
                    -- first find actuals for regions with data for only a single country
                    SELECT  region_id, 
                    MIN(d.value) AS score, 1 AS n    -- note this means MIN == d.value
                    FROM d
                    JOIN cntry_rgn AS r USING (country_id)
                    GROUP BY region_id
                    HAVING COUNT(*) = 1
                    UNION
                    -- now aggregate (with weighted average by area) regions with data
                    -- for more than one country
                    SELECT  region_id, 
                    SUM(d.value * d.country_area_km2)/SUM(d.country_area_km2) AS score, COUNT(*) AS n
                    FROM d
                    JOIN cntry_rgn AS r USING (country_id)
                    GROUP BY region_id
                    HAVING COUNT(*) > 1
  ) ORDER BY region_id")
  
  # merge the results so that available data is used when present,
  # otherwise use r2, r1, or r0 geomeans, in that order
  t_scores = sqldf(
    "SELECT r.region_id, 
    CASE  WHEN d.score IS NOT NULL     THEN d.score
    WHEN g.r2_mean IS NOT NULL   THEN g.r2_mean
    WHEN g.r1_mean IS NOT NULL   THEN g.r1_mean
    ELSE g.r0_mean
    END AS score,
    CAST(
    CASE WHEN d.score IS NOT NULL THEN 'actual'
    ELSE 'georegion'
    END AS VARCHAR(80)) AS source
    FROM rgn_georegions r
    LEFT JOIN t_actuals d USING (region_id)
    LEFT JOIN t_regionals g USING (r0, r1, r2)
    WHERE (d.region_id IS NOT NULL OR g.r0 IS NOT NULL) -- must have *some* data
    ORDER BY r.region_id")
  
  # return
  df.out = setNames(t_scores[,c('region_id','score')], c('region_id', col.value))
  attr(df.out, 'source') = t_scores[,'source']
  return(df.out)
}

aggregate_by_country_weighted = function(df, w, col.value='value', col.country='country_id', col.weight='weight', ld.csv=layers_data.csv){
  library(sqldf)
  
  # eg LIV in calc.LE:
  #  a = aggregate_weighted(df=subset(s, component='livelihood'),
  #                          w=subset(cy, year==workforce_year & !is.na(workforce), c(country_id,workforce)), 
  #                          col.value='score', col.country='country_id', col.weight='workforce') # ABW workforce==NA
  
  # set common names to rgn_georegions
  #rgn_georegions = setNames(rgn_georegions, c('region_id', 'n', 'r0', 'r1', 'r2'))
  load.regions.countries() # makes: cntry_georegions  
  
  # standardize names, first limiting to only used fields
  w  =  w[,c(col.country,col.weight)]
  df = df[,c(col.country,col.value)]
  names(w)[names(w)==col.weight] = 'w'
  names(df)[names(df)==col.value] = 'value'
  names(df)[names(df)==col.country] = 'country_id'
  
  # get data joined to georegions
  q = sprintf("SELECT d.country_id, d.value, w.w, g.r0, g.r1, g.r2
              FROM df AS d
              JOIN cntry_georegions AS g USING (country_id)
              JOIN w USING (country_id)
              WHERE value IS NOT NULL
              ORDER BY country_id")
  d = sqldf(q)
  
  # aggregate into regional area-weighted averages based on actual data
  t_regionals = sqldf(
    "SELECT * FROM (
    -- aggregate into r0 (world) georegion
    SELECT r0, SUM(value * w)/SUM(w) AS r0_mean, COUNT(*) AS r0_n
    FROM d
    GROUP BY r0
  ) AS t0 JOIN (
    -- aggregate into r1 (continent) georegions
    SELECT r0, r1, SUM(value * w)/SUM(w) AS r1_mean, COUNT(*) AS r1_n
    FROM d
    GROUP BY r0, r1
  ) AS t1 USING (r0) JOIN (
    -- aggregate into r2 (regional) georegions
    SELECT r0, r1, r2, SUM(value * w)/SUM(w) AS r2_mean, COUNT(*) AS r2_n
    FROM d
    GROUP BY r0, r1, r2
  ) AS t2 USING (r0, r1) 
    ORDER BY r0, r1, r2")  
  
  # TODO: generalize aggregate_*() functions to accept country, year, weights with code above, and use same code below.
  
  # calculate OHI region area weighted average of values using available data
  t_actuals = sqldf(
    "SELECT * FROM  (    
    -- first find actuals for regions with data for only a single country
    SELECT  region_id, 
    MIN(d.value) AS score, 1 AS n    -- note this means MIN == d.value
    FROM d
    JOIN cntry_rgn AS r USING (country_id)
    GROUP BY region_id
    HAVING COUNT(*) = 1
    UNION
    -- now aggregate (with weighted average by area) regions with data
    -- for more than one country
    SELECT  region_id, 
    SUM(d.value * d.w)/SUM(d.w) AS score, COUNT(*) AS n
    FROM d
    JOIN cntry_rgn AS r USING (country_id)
    GROUP BY region_id
    HAVING COUNT(*) > 1
  ) ORDER BY region_id")
  
  # merge the results so that available data is used when present,
  #   otherwise use r2, r1, or r0 geomeans, in that order
  t_scores = sqldf(
    "SELECT r.region_id, 
    CASE  WHEN d.score IS NOT NULL     THEN d.score
    WHEN g.r2_mean IS NOT NULL   THEN g.r2_mean
    WHEN g.r1_mean IS NOT NULL   THEN g.r1_mean
    ELSE g.r0_mean
    END AS score,
    CAST(CASE WHEN d.score IS NOT NULL THEN 'actual'
    ELSE 'georegion'
    END AS VARCHAR(80)) AS source
    FROM rgn_georegions r
    LEFT JOIN t_actuals d USING (region_id)
    LEFT JOIN t_regionals g USING (r0, r1, r2)
    WHERE (d.region_id IS NOT NULL OR g.r0 IS NOT NULL) -- must have *some* data
    ORDER BY r.region_id")
  
  # return
  df.out = setNames(t_scores[,c('region_id','score')], c('region_id', col.value))
  attr(df.out, 'source') = t_scores[,'source']
  return(df.out)
}

aggregate_by_country_year = function(df, col.value='value', col.country='country_id'){
  library(sqldf)
  
  # debug: df = cny_k = subset(cnky, product=='fish_oil'); col.value='Xp'; col.country='country_id'
  #rgn_georegions = setNames(rgn_georegions, c('region_id', 'n', 'r0', 'r1', 'r2'))
  load.regions.countries(layers_data.csv) # makes: cntry_georegions  
  
  # get data joined to georegions
  q = sprintf("SELECT d.%s AS country_id, d.year, c.country_area_km2, d.%s AS value, g.r0, g.r1, g.r2
              FROM df AS d
              JOIN cntry_georegions AS g USING (country_id)
              JOIN cntry AS c USING (country_id)              
              WHERE value IS NOT NULL
              ORDER BY country_id", col.country, col.value)
  d = sqldf(q)
  
  # aggregate into regional area-weighted averages based on actual data
  t_regionals = sqldf(
    "SELECT * FROM (
    -- aggregate into r0 (world) georegion
    SELECT year, r0, SUM(value * country_area_km2)/SUM(country_area_km2) AS r0_mean, COUNT(*) AS r0_n
    FROM d
    GROUP BY year, r0
  ) AS t0 JOIN (
    -- aggregate into r1 (continent) georegions
    SELECT year, r0, r1, SUM(value * country_area_km2)/SUM(country_area_km2) AS r1_mean, COUNT(*) AS r1_n
    FROM d
    GROUP BY year, r0, r1
  ) AS t1 USING (year, r0) JOIN (
    -- aggregate into r2 (regional) georegions
    SELECT year, r0, r1, r2, SUM(value * country_area_km2)/SUM(country_area_km2) AS r2_mean, COUNT(*) AS r2_n
    FROM d
    GROUP BY year, r0, r1, r2
  ) AS t2 USING (year, r0, r1)")
  
  # calculate OHI region area weighted average of values using available data
  t_actuals = sqldf("SELECT * FROM  (    
                    -- first find actuals for regions with data for only a single country
                    SELECT d.year, region_id, 
                    MIN(d.value) AS score, 1 AS n    -- note this means MIN == d.value
                    FROM d
                    JOIN cntry_rgn AS r USING (country_id)
                    GROUP BY region_id, d.year
                    HAVING COUNT(*) = 1
                    UNION
                    -- now aggregate (with weighted average by area) regions with data
                    -- for more than one country
                    SELECT d.year, region_id, 
                    SUM(d.value * d.country_area_km2)/SUM(d.country_area_km2) AS score, COUNT(*) AS n
                    FROM d
                    JOIN cntry_rgn AS r USING (country_id)
                    GROUP BY region_id, d.year
                    HAVING COUNT(*) > 1
  ) ORDER BY region_id")
  
  # merge the results so that available data is used when present,
  # otherwise use r2, r1, or r0 geomeans, in that order
  t_scores = sqldf(
    "SELECT d.year, r.region_id, 
    CASE  WHEN d.score IS NOT NULL     THEN d.score
    WHEN g.r2_mean IS NOT NULL   THEN g.r2_mean
    WHEN g.r1_mean IS NOT NULL   THEN g.r1_mean
    ELSE g.r0_mean
    END AS score,
    CAST(
    CASE WHEN d.score IS NOT NULL THEN 'actual'
    ELSE 'georegion'
    END AS VARCHAR(80)) AS source
    FROM rgn_georegions r
    LEFT JOIN t_actuals d USING (region_id)
    LEFT JOIN t_regionals g USING (year, r0, r1, r2)
    WHERE (d.region_id IS NOT NULL OR g.r0 IS NOT NULL) -- must have *some* data
    ORDER BY d.year, r.region_id")
  
  # return
  df.out = setNames(t_scores[,c('year','region_id','score')], c('year', 'region_id', col.value))
  attr(df.out, 'source') = t_scores[,'source']
  return(df.out)
}
