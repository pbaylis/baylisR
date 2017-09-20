#' Clean binned results from felm (and maybe lm?). Will try to guess at omitted variable from variable names created using cut().
#' @param dt data.table to summarize.
#' @param i.names Character vector of variables in dt.file that identify 
#' @param t.name Character vector of length 1 that identifies date variable.
#' @param bins.seqs List of named sequences that define bin variables.
#' @param mean.names Character vector of variables to take mean over.
#' @param sum.names Character vector of variables to sum over.
#' @return Monthly summary of variables from PRISM.
#' @import data.table
#' @import zoo
#' @export
# Takes in a data.table with daily weather data and produces aggregated data.table by month of sample using requested summary functions.
prism_daily_to_ym <- function(dt, 
                         i.names, 
                         t.name=c("date"), 
                         mean.names=c("tmin"),
                         sum.names=c("ppt"),
                         bins.seqs=list(tmax=c(-Inf, seq(0, 40, 10), Inf), tmin=c(-Inf, seq(-10,30,10), Inf))) {
  
  ### TESTING - 
  # dt <- readRDS("/Users/pbaylis/Dropbox/01_Work/01_Current/99_Common/prism_code/prism_areacode_2002.Rds")
  # i.names=c("NPA")
  # t.name=c("date")
  # bins.seqs = list(tmax=c(-Inf, seq(-10, 60, 5), Inf))
  # mean.names=c("ppt")
  # sum.names=c("tmin", "tmax")
  ### END TESTING
  
  dt[, ym:=as.yearmon(get(t.name))]

  # Create unique identifer, will merge original unit variables back in
  dt[, i:=.GRP, by=i.names]
  i.mapping <- dt[, .N, by=c(i.names, "i")]
  
  # List to hold aggregated data
  agg.list <- list()
  
  days_aggregated <- dt[, list(days_aggregated=.N), by=list(i, ym)]
  agg.list[["days_aggregated"]] <- days_aggregated
  
  # Means
  means <- dt[, lapply(.SD, mean, na.rm=T), by=list(i, ym), .SDcols=mean.names]
  setnames(means, mean.names, paste0(mean.names, "_mean"))
  agg.list[["means"]] <- means
    
  # Sums
  sums <- dt[, lapply(.SD, sum, na.rm=T), by=list(i, ym), .SDcols=sum.names]
  setnames(sums, sum.names, paste0(sum.names, "_sum"))
  agg.list[["sums"]] <- sums
  
  # Bin counts
  for (bins.name in names(bins.seqs)) {
    # bins.name <- "tmax"
    dt[, temp.cut:=cut(get(bins.name), bins.seqs[[bins.name]])]
    bins <- dt[, .N, by=list(i, ym, temp.cut)]
    bins[, temp.cut:=paste0(bins.name, "_", temp.cut)]
    bins.wide <- dcast(bins, i + ym ~ temp.cut, value.var="N", fill=0) # Reshape wide
    bin.col.names <- paste0(bins.name, "_", levels(dt$temp.cut))
    # To ensure that all requested levels of cut variables are populated, add if they don't exist.
    # This may cause issues if there are bins with decimals in them (i.e., if the program guesses wrong names for cut vars.)
    for (bin.col.name in bin.col.names) { 
      if (!(bin.col.name %in% names(bins.wide))) {
        bins.wide[, c(bin.col.name):=0]
      }
    }
    setcolorder(bins.wide, c("i", "ym", bin.col.names)) # Reorder columns
    agg.list[[paste0(bins.name, "_bins")]] <- bins.wide
  }
  
  
  # Merge aggregates together
  agg <- Reduce(function(x, y) merge(x, y, by=c("i", "ym"), all=T), agg.list)
  
  # Add old ids back and reorder
  agg <- merge(agg, i.mapping[, c("i", i.names), with=F], by="i", all=T)
  agg[, i:=NULL]
  top.vars <- c(i.names, "ym")
  setcolorder(agg, c(top.vars, setdiff(names(agg), top.vars)))
  
  return(agg)
}


# DEBUG
# dt.file<-"prism_areacode_2002.Rds"; i.names <- c("NPA"); t.name <- c("date");
# bins.seqs <- list(tmax=c(-Inf, seq(0, 40, 10), Inf), tmin=c(-Inf, seq(-10,30,10), Inf));
# sum.names = c("ppt", "tmin");
# 
# test <- 