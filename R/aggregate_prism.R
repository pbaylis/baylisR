#' Generalized version of PRISM_ym
#' Takes in a data.table with daily weather data and produces aggregated data.table by month of sample using requested summary functions.
#' @param dt data.table to summarize.
#' @param i.names Character vector of variables in dt.file that identify 
#' @param t.in Character vector of length 1 that identifies input time variable.
#' @param t.fun Single item list, name 1 is grouping name and item 1 is grouping function to execute on t.in 
#' @param bins.seqs List of named sequences that define bin variables.
#' @param mean.names Character vector of variables to take mean over.
#' @param sum.names Character vector of variables to sum over.
#' @return Monthly summary of variables from PRISM.
#' @import data.table
#' @import zoo
#' @export
aggregate_prism <- function(dt, 
                              i.names, 
                              t.in=c("date"), 
                              t.fun=as.yearmon,
                              mean.names=c("tmin"),
                              sum.names=c("ppt"),
                              bins.seqs=list(tmax=c(-Inf, seq(0, 40, 10), Inf), 
                                             tmin=c(-Inf, seq(-10,30,10), Inf))) {
  
  ### TESTING: DAILY TO YM- 
  # dt <- readRDS("B:/Dropbox/01_Work/01_Current/99_Common/prism_code/prism_areacode_2002.Rds")
  # i.names=c("NPA")
  # t.in <- c("date")
  # t.fun <- list(ym=as.yearmon)
  # bins.seqs = list(tmax=c(-Inf, seq(-10, 60, 5), Inf))
  # mean.names=c("ppt")
  # sum.names=c("tmin", "tmax")
  # ### END TESTING
  
  ### TESTING: YM TO DECADAL
  # dt <- weather
  # i.names <- c("fips")
  # t.in <- "ym"
  # t.fun <- list(decade=function(x) ceiling(floor(as.numeric(x))/10)*10)
  # bins.seqs = list(tmean1=c(-Inf, seq(-5, 25, 5), Inf),
  #                  tmean2=c(-Inf, seq(-6, 28, 3), Inf),
  #                  tmean3=c(-Inf, seq(-5, 28, 1), Inf))
  # mean.names=c("ppt")
  # sum.names=c("tmean")
  # END
  
  # Create temporal aggregation indicator
  dt[, t:=t.fun[[1]](get(t.in))]
  
  # Create unique identifer, will merge original unit variables back in
  dt[, i:=.GRP, by=i.names]
  i.mapping <- dt[, .N, by=c(i.names, "i")]
  
  # List to hold aggregated data
  agg.list <- list()
  
  N_aggregated <- dt[, list(N_aggregated=.N), by=list(i, t)]
  agg.list[["N_aggregated"]] <- N_aggregated
  
  # Means
  means <- dt[, lapply(.SD, mean, na.rm=T), by=list(i, t), .SDcols=mean.names]
  setnames(means, mean.names, paste0(mean.names, "_mean"))
  agg.list[["means"]] <- means
  
  # Sums
  sums <- dt[, lapply(.SD, sum, na.rm=T), by=list(i, t), .SDcols=sum.names]
  setnames(sums, sum.names, paste0(sum.names, "_sum"))
  agg.list[["sums"]] <- sums
  
  # Bin counts
  for (bins.name in names(bins.seqs)) {
    # bins.name <- "tmax"
    var.name <- gsub("[0-9]*", "", bins.name) # Remove numbers
    dt[, temp.cut:=cut(get(var.name), bins.seqs[[bins.name]])]
    bins <- dt[!(is.na(temp.cut)), .N, by=list(i, t, temp.cut)]
    bins[, temp.cut:=paste0(bins.name, "_", temp.cut)]
    bins.wide <- dcast(bins, i + t ~ temp.cut, value.var="N", fill=0) # Reshape wide
    bin.col.names <- paste0(bins.name, "_", levels(dt$temp.cut))
    # To ensure that all requested levels of cut variables are populated, add if they don't exist.
    # This may cause issues if there are bins with decimals in them (i.e., if the program guesses wrong names for cut vars.)
    for (bin.col.name in bin.col.names) { 
      if (!(bin.col.name %in% names(bins.wide))) {
        bins.wide[, c(bin.col.name):=0]
      }
    }
    setcolorder(bins.wide, c("i", "t", bin.col.names)) # Reorder columns
    agg.list[[paste0(bins.name, "_bins")]] <- bins.wide
  }
  
  
  # Merge aggregates together
  agg <- Reduce(function(x, y) merge(x, y, by=c("i", "t"), all=T), agg.list)
  
  # Add old ids back and reorder
  agg <- merge(agg, i.mapping[, c("i", i.names), with=F], by="i", all=T)
  agg[, i:=NULL]
  top.vars <- c(i.names, "t")
  setcolorder(agg, c(top.vars, setdiff(names(agg), top.vars)))
  setnames(agg, "t", names(t.fun)[1])
  return(agg)
}

