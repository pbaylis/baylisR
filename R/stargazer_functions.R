#' Clean up stargazer output
#' @param stargazer.out Output from stargazer
#' @param replace.stars.with.sym Boolean to replace stars with sym command.
#' @return Cleaned-up table output.
#' @export
stargazer_clean <- function(stargazer.out, replace.stars.with.sym=F) {  # Just get the stuff inside tabular environment from stargazer
  # Trim to just the table
  first.line <- grep("begin\\{tabular\\}", stargazer.out) + 1
  last.line <- grep("end\\{tabular\\}", stargazer.out) - 1
  stargazer.out <- stargazer.out[first.line:last.line]
  # Remove \\hline rows
  hline.rows <- grep("\\hline", stargazer.out)
  if (length(hline.rows) > 0) {
    stargazer.out <- stargazer.out[-hline.rows]
  }
  # Make all multicolumns align left (should be variable names)
  stargazer.out <- gsub("multicolumn\\{1\\}\\{c\\}", "multicolumn\\{1\\}\\{l\\}", stargazer.out)
  
  # Switch to sym for estauto
  if (replace.stars.with.sym) {
    stargazer.out <- gsub("\\^\\{", "\\\\sym\\{", stargazer.out)
  }
  
  # Sometimes there are rows that are like " & & & ... & \\\\" - drop them
  stargazer.out <- stargazer.out[sapply(strsplit(stargazer.out, ""), function(x) !all(unique(x) %in% c(" ", "&", "\\")))]
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