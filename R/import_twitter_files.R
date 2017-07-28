import_twitter_files <- function(inFiles, col_types=NULL, rast.file=NULL, keep_UTC=FALSE) {
  #' This function imports many CSVs of twitter data and collects them into a single data.table.
  #' @param inFiles Set of files to import. Presumably a list of .csv or .csv.gz.
  #' @param col_types Passed through to read_csv. NULL imports all fields.
  #' @param keep_UTC Flag to return UTC datetime.
  #' @return Returns data.table.
  #' import_twitter_files()
  require(raster)
  require(readr)
  inFiles <- list("B:/twitter_data/usa_geo/scores/usa_geo_backup_2014-05-22_scores.csv")
  col_types <- cols_only(tweet_datetime=col_datetime(format = ""),
                      lng=col_double(), lat=col_double(),
                      afinn="?", vaderPos="?", vaderNeg="?")
  rast.file <- "B:/Dropbox/01_Work/01_Current/99_Common/geo/MODIS1k_rasterized.tif"
  rast <- raster(rast.file)
  return(rbindlist(lapply(inFiles, import_twitter_csv, col_types=col_types, rast=rast, keep_UTC=keep_UTC),
                   use.names=TRUE))
}

import_twitter_file <- function(inFile, col_types=NULL, rast=NULL, keep_UTC=FALSE) {
  #' This function imports a single CSV of twitter data.
  #' @param inFile Filename to import (.csv or .csv.gz).
  #' @param col_types Passed through to read_csv. NULL imports all fields.
  #' @param keep_UTC Flag to return UTC datetime.
  #' @return Returns data.table.
  #' import_twitter_file()
  require(data.table)
  require(raster)
  require(readr)
  tic <- Sys.time()
  
  inFile <- "B:/twitter_data/usa_geo/scores/usa_geo_backup_2014-05-22_scores.csv"
  col_types <- cols_only(tweet_datetime=col_datetime(format = ""),
                         lng=col_double(), lat=col_double(),
                         afinn="?", vaderPos="?", vaderNeg="?")
  keep_UTC <- TRUE
  
  if (file.exists(inFile)) {
    print(paste0("Loading ", inFile))

    system.time(scores <- as.data.table(read_csv(inFile, col_types=col_types)))

    # Change postedTime or tweet_datetime to datetimeUTC
    if ("postedTime" %in% names(scores)) {setnames(scores, "postedTime", "datetimeUTC")}
    if ("tweet_datetime" %in% names(scores)) {setnames(scores, "tweet_datetime", "datetimeUTC")}

    # Get MODIS raster data
    scores[, cell.MODIS1k := cellFromXY(rast, scores[, list(lng, lat)])]

    scores[, TZID:=test <- getValues(rast)[cell.MODIS1k]] 
    scores[, `:=`(date=as.IDate(datetimeUTC, tz=TZID)), by=TZID] # Get local date (could get time but aggregating anyway)

    print(Sys.time() - tic)
    return(scores)
  }
  print(paste0(inFile, " did not exist or had no data"))
  return(NULL)
}

# another option - just have a fxn here for importing one twitter file. then have another utility fxn for 
# adding data via a raster. be good to test if that's not too memory intensive on massive datasets.