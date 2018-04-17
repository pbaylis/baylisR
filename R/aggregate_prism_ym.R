#' Aggregate PRISM file by ym
#' @param dt.file filename of .Rds with data.table
#' @param i.names Name(s) of ID variable
#' @param t.name Name of time variable 
#' @param bincount.seqs Named list of sequences for cut variables. Name should be the relevant variable name.
#' @param mean.names Character vector of names of variables to take means over
#' @param sum.names Character vector of names of variables to take sums over
#' @return data.table of aggregated data
#' @import data.table
#' @import zoo
#' @export
aggregate_prism_ym <- function(dt.file, i.names, t.name, bincount.seqs=NULL, mean.names=NULL, sum.names=NULL) {
  # DEBUG
  #dt.file<-"E:/prism/data/areacode_popweighted_final/prism_areacode_2002.Rds"; i.names <- c("NPA"); t.name <- c("date");
  #bincount.seqs <- list(tmax=c(-Inf, seq(0, 40, 2), Inf), tmin=c(-Inf, seq(-10,30,10), Inf));
  #sum.names = c("ppt", "tmin");
  print(dt.file)
  dt <- readRDS(dt.file)
  dt[, ym:=as.yearmon(get(t.name))]
  
  # Create unique identifer, will merge original unit variables back in
  dt[, i:=.GRP, by=i.names]
  i.mapping <- dt[, .N, by=c(i.names, "i")]
  
  # Bin counts
  agg.list <- list()
  for (bincount.name in names(bincount.seqs)) {
    # bincount.name <- "tmax"
    # print(bincount.name)
    dt[, temp.cut:=cut(get(bincount.name), bincount.seqs[[bincount.name]], ordered_result=TRUE)]
    bins <- dt[, .N, by=list(i, ym, temp.cut)]
    bins[, bin.order:=as.numeric(temp.cut)]
    max.width <- ceiling(log10(max(bins$bin.order, na.rm = T)))
    bins[, bin.order:=formatC(bin.order, width=max.width, format="d", flag="0")] # Prepend 0s
    bins[, temp.cut:=paste0(bincount.name, "_bin",bin.order,"_", temp.cut)]
    # Reshape wide
    bins.wide <- dcast(bins, i + ym ~ temp.cut, value.var="N", fill=0)
    agg.list[[paste0(bincount.name, "_bins")]] <- bins.wide
  }
  
  # Means
  if (!is.null(mean.names)) {
    means <- dt[, lapply(.SD, mean, na.rm=T), by=list(i, ym), .SDcols=mean.names]
    setnames(means, mean.names, paste0(mean.names, "_mean"))
    agg.list[["means"]] <- means
  }
  
  
  # Sums
  if (!is.null(sum.names)) {
    sums <- dt[, lapply(.SD, sum, na.rm=T), by=list(i, ym), .SDcols=sum.names]
    setnames(sums, sum.names, paste0(sum.names, "_sum"))
    agg.list[["sums"]] <- sums
  }
  
  
  Ndays <- dt[, list(Ndays=.N), by=list(i, ym)]
  agg.list[["Ndays"]] <- Ndays
  
  # Merge aggregates together
  agg <- Reduce(function(x, y) merge(x, y, by=c("i", "ym"), all=T), agg.list)
  
  # Add old ids back and reorder
  agg <- merge(agg, i.mapping[, c("i", i.names), with=F], by="i", all=T)
  agg[, i:=NULL]
  setcolorder(agg, c(i.names, "ym", setdiff(names(agg), c(i.names, "ym"))))
  
  return(agg)
}
# Usage ex: 
# agg <- rbindlist(lapply(list.files("prism_zcta", full.names=T), aggregate_prism_ym, 
#                         i.name=c("zcta"), t.name=c("date"),
#                         bincount.seqs <- list(tmax=c(-Inf, seq(0, 40, 5), Inf)),
#                         mean.names = c("tmin", "tmax"),
#                         sum.names = c("ppt")))