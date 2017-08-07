#' Clean binned results from felm (and maybe lm?). Will try to guess at omitted variable from variable names created using cut().
#' @param felm.est felm output from regression with binned variables, can be stripped with strip_felm. Required if felm.dt not supplied.
#' @param felm.dt data.table of output from regression with binned variables. Required if felm.est not supplied.
#' @param keepvars (Optional) regex of variables to keep.
#' @return Ordered data.table with coef, se, ci.l, ci.h, xmin, xmax, xmid. Will include a row for the (guessed at) omitted variable with coef and se = 0.
#' @import data.table
#' @import lfe
#' @export
clean_binned_results <- function(felm.est=NULL, felm.dt=NULL, keepvars=NULL) {
  if (!is.null(felm.est)) {
    dt <- felm_to_dt(felm.est, keepvars=keepvars)
  } else if (!is.null(felm.dt)) {
    dt <- felm.dt
  } else {
    stop("Requires felm.est or felm.dt.")
  }

  # Clean and order coefficient data.tables
  dt[, c("varname", "xmin", "xmax"):=extract_from_range(dt$varname.full)]

  # Create midpoint and "fake" xmin and xmax for plotting
  gaps <- dt$xmax-dt$xmin
  gap <- mean(gaps[is.finite(gaps) & !is.na(gaps)])
  dt[, `:=`(xmin.true=xmin, xmax.true=xmax)] # Save infinite

  dt[!is.finite(xmax), xmax:=xmin + gap]
  dt[!is.finite(xmin), xmin:=xmax - gap]

  dt <- rbind(dt,
              dt[, list(xmin=find_missing(xmin, gap),
                        xmax=find_missing(xmax, gap),
                        coef=0, se=0), by=varname], fill=T)

  # Assume lowest bin is missing if no gaps were found
  dt[, `:=`(xmin.temp=min(xmin, na.rm=T) - gap,
            xmax.temp=min(xmin, na.rm=T)), by=varname]

  dt[is.na(xmin) & is.na(xmax), `:=`(xmin=xmin.temp,
                                     xmax=xmax.temp), by=varname]
  dt[, `:=`(xmin.temp=NULL, xmax.temp=NULL)]

  # Fill in missing truth values
  dt[is.na(xmin.true), xmin.true:=xmin]
  dt[is.na(xmax.true), xmax.true:=xmax]

  dt[, `:=`(xmid=(xmin + xmax)/2,
            ci.l=coef - se*1.96,
            ci.h=coef + se*1.96)]

  setorder(dt, varname, xmid) # Do I need setcolorder?
  return(dt)
}

#' # By varname, fill in missing value, assuming constant gap. Used by clean_binned_results, not for export right now.
#' @param x Numeric vector
#' @param gap Numeric of gap between each x.
#' @return Numeric of missing x value.
find_missing <- function(x, gap) {
  full <- seq(min(x), max(x), gap)
  missing <- full[!full %in% x]
}

#' # Convert felm to data.table. Not exported for now.
#' @param felm.est felm object.
#' @param keepvars regex of variables to keep.
#' @return data.table of results
#' @import data.table
felm_to_dt <- function(felm.est, keepvars=NULL) {
  dt <- as.data.table(summary(felm.est)$coefficients[, 1:2], keep.rownames=T)
  setnames(dt, c("varname.full", "coef", "se"))
  if (!is.null(keepvars)) {
    dt <- dt[grep(keepvars, varname.full)]
  }
  return(dt)
}

#' Get varname, min, max from named range. Not exported
#' @param x Character vector.
#' @return Named list with varname, xmin, xmax
#' @import stringr
extract_from_range <- function(x) {
  match.res <- str_match(x, "([^\\(\\[]*)[\\(\\[]([-Inf0-9 ]+),([-Inf0-9 ]+)[\\)\\]]")

  # We'll be adding to existing data table
  return(list(varname=match.res[, 2],
              xmin=as.numeric(match.res[, 3]),
              xmax=as.numeric(match.res[, 4])))
}
