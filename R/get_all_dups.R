#' Get all duplicates in a data.table for a given set of variables
#' @param dt data.table 
#' @param by character vector of variable names to use as ID columns for duplicate check
#' @return Returns data table of duplicates
#' @import data.table
#' @export
get_all_dups <- function(dt, by) {
  # Utility fxn to return all duplicates from a dataet
  dups <- dt[duplicated(dt, by=by) | duplicated(dt, by=by, fromLast=T)]
  setorderv(dups, by)
  return(dups)
}