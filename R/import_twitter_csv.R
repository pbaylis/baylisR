import_twitter_files <- function(inFiles, col_types=NULL, rast.file=NULL, keep_UTC=FALSE) {
  #' This function imports many CSVs of twitter data and collects them into a single data.table.
  #' @param inFiles Set of files to import. Presumably a list of .csv or .csv.gz.
  #' @param col_types Passed through to read_csv. NULL imports all fields.
  #' @param keep_UTC Flag to return UTC datetime.
  #' @return Returns data.table.
  #' import_twitter_dir()
  require(raster)
  rast <- raster(rast.file)
  return(rbindlist(lapply(inFiles, import_twitter_csv, col_types=col_types, rast=rast, keep_UTC=keep_UTC)))
}

import_twitter_csv <- function(inFile, col_types=NULL, rast=NULL, keep_UTC=FALSE) {
  #' This function imports a single CSV of twitter data.
  #' @param inFile Filename to import (.csv or .csv.gz).
  #' @param col_types Passed through to read_csv. NULL imports all fields.
  #' @param keep_UTC Flag to return UTC datetime.
  #' @return Returns data.table.
  #' import_twitter_csv()
  require(data.table)
  require(raster)
  require(readr)
  tic <- Sys.time()
  if (file.exists(inFile)) {
    print(paste0("Loading ", inFile))
    
    system.time(scores <- as.data.table(read_csv(inFile, col_types=col_types)))
    
    # Change postedTime or tweet_datetime to datetimeUTC
    if ("postedTime" %in% names(scores)) {setnames(scores, "postedTime", "datetimeUTC")}
    if ("tweet_datetime" %in% names(scores)) {setnames(scores, "tweet_datetime", "datetimeUTC")}
    
    # Get MODIS raster data
    scores[, cell.MODIS1k := cellFromXY(rast.MODIS1k, scores[, list(lng, lat)])]
    
    scores[, TZID:=as.character(TZID)] # Might not be necessary?
    scores[, `:=`(date=as.IDate(datetimeUTC, tz=TZID)), by=TZID] # Get local date (could get time but aggregating anyway)
    
    print(Sys.time() - tic)
    return(scores)
  }
  print(paste0(inFile, " did not exist or had no data"))
  return(NULL)
}