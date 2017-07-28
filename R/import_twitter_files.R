#' This function imports a single CSV of twitter data.
#' @param inFile Filename to import (.csv or .csv.gz).
#' @param col_types Passed through to read_csv. NULL imports all fields.
#' @return Returns data.table.
#' @import data.table
#' @import readr
#' @export
import_twitter_file <- function(inFile, col_types=NULL) {
  if (file.exists(inFile)) {
    print(paste0("Loading ", inFile))

    system.time(scores <- as.data.table(read_csv(inFile, col_types=col_types)))

    # Change postedTime or tweet_datetime to datetimeUTC
    if ("postedTime" %in% names(scores)) {setnames(scores, "postedTime", "datetimeUTC")}
    if ("tweet_datetime" %in% names(scores)) {setnames(scores, "tweet_datetime", "datetimeUTC")}
    return(scores)
  }
  print(paste0(inFile, " did not exist or had no data"))
  return(NULL)
}

#' Gets local time for a data.table with lat, lng, datetimeUTC
#' @param dt data.table to import, must have following cols (at minimum): lng, lat, tweet_datetime.
#' @param debug Flag to also return raster cell, value, and TZID.
#' #' @return Returns data.table.
#' @import data.table
#' @importFrom raster cellFromXY getValues
#' @export
get_local_time <- function(dt, debug=FALSE) {
  # tz.rast is silently loaded from sysdata.rda
  dt[, rast.cell := cellFromXY(tz.rast, dt[, list(lng, lat)])]
  dt[, rast.value := getValues(tz.rast)[rast.cell]]
  
  dt[, TZID := tz.rast@data@attributes[[1]]$category[match(rast.value, tz.rast@data@attributes[[1]]$ID)]]

  dt[!is.na(TZID), `:=`(date=as.IDate(datetimeUTC, tz=TZID),
                        time=as.ITime(datetimeUTC, tz=TZID)), 
     by=list(TZID=as.character(TZID))] 
  
  if(debug == TRUE) {
    dt[, `:=`(rast.cell=NULL, rast.value=NULL, TZID=NULL)] # clean up
  }
  
  return(dt) # Good practice, not necessary
}