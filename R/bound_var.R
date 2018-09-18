#' Constrain values of a numeric vector within lower and upper limits
#' @param x Numeric vector
#' @param x_min Minimum allowable value of x
#' @param x_max Maximum allowable value of x
#' @return Returns numeric vector within all values of x  x_min and x_max
#' @export
bound_var <- function(x, x_min = 0, x_max = 30000) { # Add this bad
  x <- pmin(pmax(x, x_min), x_max)
  return(x)
}  # topcode(c(-5, 5, 10006))
