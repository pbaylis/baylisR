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

    print(Sys.time() - tic)
    return(scores)
  }
  print(paste0(inFile, " did not exist or had no data"))
  return(NULL)
}

add_raster_data <- function(dt, rast, rast.levels=NULL) {
  require(raster)
  scores[, cell.rast := cellFromXY(rast, scores[, list(lng, lat)])]

  scores[, rast.value:=getValues(rast)[cell.rast]]

  # Replace numeric with levels
  if (!is.null(rast.levels)) {
    scores[, rast.value2:=rast.levels[rast.value]]
    scores[, rast.value:=NULL]
    setnames(scores, "rast.value2", "rast.value")
  }

}

get_local_time <- function(dt) {
  # dt with following cols (at minimum): lng, lat, tweet_datetime
  # return same dt plus cell plus local date and time
  require(fasterize)
  require(sf)
  require(raster)
  MODIS.rast <- raster(MODIS.file) # Is there a reliable way to include data in packages?
  tz.shp <-
  tz.rast <- fasterize(MODIS.rast, tz.shp)
  rast.fact <- levels(tz.rast)
  add_raster_data(dt, tz.rast, rast.levels)


  scores[, `:=`(date=as.IDate(datetimeUTC, tz=TZID)), by=TZID] # Get local date (could get time but aggregating anyway)

}
