#' Identify rows with duplicates
#' @param x data.frame or data.table
#' @param by character vector of variables over which to check duplicates
#' @return boolean vector, where TRUE indicates that at least one other row in x has the same values for by
#' @export
duplicated_all <- function(x, by) { # Augmented duplicated(), value is a boolean vector
  return(duplicated(x, by = by) | duplicated(x, by = by, fromLast = T))
}
