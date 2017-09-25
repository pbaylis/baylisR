#' Calls stargazer, returns cleaned up stargazer output. Sets a few other defaults.
#' @param ... Parameters for stargazer
#' @return Character vector of cleaned-up table output. 
#' @import stargazer
#' @export
stargazerw <- function(..., align=T, table.layout="c-t", no.space=T, style="aer", fragment=F, drop.hline=F) {
  stargazer.raw <- stargazer(..., align=align, table.layout=table.layout, no.space=no.space, style=style)
  
  stargazer.out <- stargazer.raw
  
  offset <- as.numeric(fragment) # If fragment is true, tabular row will get dropped
  first.line <- grep("begin{tabular}", stargazer.out, fixed=T) + offset
  last.line <- grep("end{tabular}", stargazer.out, fixed=T) - offset
  stargazer.out <- stargazer.out[first.line:last.line]
  
  # Replace \hline with \midrule if not dropping
  if (drop.hline == FALSE) {
    stargazer.out <- gsub("\\hline", "\\midrule", stargazer.out, fixed=T)
  } else {
    hline.rows <- grep("\\hline", stargazer.out, fixed=T)
    if (length(hline.rows) > 0) {
      stargazer.out <- stargazer.out[-hline.rows]
    }
  }

  # Insert \toprule and \bottomrule if this isn't a fragment
  if (fragment == FALSE ) {
    stargazer.out <- c(stargazer.out[-length(stargazer.out)], "\\bottomrule", stargazer.out[length(stargazer.out)])
    stargazer.out <- c(stargazer.out[1], "\\toprule", stargazer.out[-1])
  }
  
  # Remove \\\\[-1.8ex]
  stargazer.out <- gsub("\\\\[-1.8ex]", "", stargazer.out, fixed=T)
  
  return(stargazer.out)
}

#' Create a LaTeX table row character vector wrapped in multicolumn.
#' @param x vector with length = to number of columns, will be coerced to character
#' @param col.widths Vector of column widths. Will be assumed to be 1 for member of x if not specified.
#' @param align Desired alignment for cells.
#' @return LaTeX table row character vector wrapped in multicolumn.
#' @export
latex_special_row <- function(x, col.widths=NULL, align=NULL) {
  if (is.null(col.widths)) {
    col.widths <- rep(1, length(x))
  }
  while (length(align) < length(x)) {
    align <- c(align, "c")
  }
  return(paste0(paste0("\\multicolumn{", col.widths,"}{", align, "}{", x, "}", collapse = " & "), " \\\\"))
}

#' Create a set of cmidrules based on given edges.
#' @param edges Should be an even number.
#' @return LaTeX character vector of cmidrules.
#' @export
latex_makesubrules <- function(edges) {
  edges.split <- split(edges, rep(1:(length(edges)/2), each=2))
  return(paste(sapply(edges.split, function(x) paste0("\\cmidrule(lr){", x[1], "-", x[2], "}")), collapse=" "))
}

#' Convenience function to format number with commas.
#' @param x Numeric vector.
#' @param digits Number of digits past decimal.
#' @return Character vector with commas.
#' @export
fC <- function(x, digits=2) {
  formatC(x, format="f", digits=digits, big.mark=",", drop0trailing=T)
}