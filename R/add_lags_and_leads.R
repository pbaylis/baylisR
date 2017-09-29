#' Return data.table with lag
#' @param dt data.table 
#' @param varnames Character vector of variables to lag or lead
#' @param tvar Name of time variable. Default is "date".
#' @param by (Optional) Name of variable to run lags/leads by
#' @param n (Optional) Number of lags/leads to create
#' @param typenames (Optional) Character vector equal to c("lag"), c("lead")", or c("lag", "lead") (default)
#' @return data.table with lags and leads added
#' @import data.table
#' @export
add_lags_and_leads <- function(dt, varnames, tvar="date", by=NULL, n=3, typenames=c("lag", "lead")) {
  setorderv(dt, cols=c(by, tvar))
  for (var.name in varnames) {
    for (type.name in typenames) {
      for (i in 1:n) {
        print(paste(var.name, type.name, i, sep="_"))
        dt[, paste(var.name, type.name, i, sep="_"):=data.table::shift(get(var.name), n=i, type=type.name), by=c(by)]
      }
    }
  }
  return(dt)
}
