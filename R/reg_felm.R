#' Wrapper for lfe::felm
#'
#' @param lhs Character string of LHS, e.g., "y"
#' @param rhs Character string of RHS, e.g., "x1 + x2"
#' @param fe Character string of RHS, e.g., "fe1 + fe2"
#' @param clus Character string of clustering variables, e.g., "clus1 + clus2"
#' @param data Symbol for data.table with data in it
#' @param weights_var Character string for the weights_var (should be found in data)
#'
#' @return Stripped felm object.
#' @import data.table
#' @import lfe
#' @export
reg_felm <- function(lhs, rhs, fe, clus, data, weights_var) {
  fmla_str <- sprintf("%s ~ %s | %s | 0 | %s", lhs, rhs, fe, clus)
  print(fmla_str)
  strip_felm(felm(as.formula(fmla_str), data = data, weights = data[, get(weights_var)]))
}
