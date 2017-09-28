#' Construct a formula for lfe::felm
#' @param lhs Character vector of length 1 with name of left-hand side variable
#' @param rhs Character vector with name(s) of right-hand side variables. If length(rhs) > 1, will be concatenated with " + " in between
#' @param fe (OPTIONAL) Character vector with name(s) of fixed effects. If length(rhs) > 1, will be concatenated with " + " in between
#' @param clus (OPTIONAL) Charactervector with name(s) of clustering variables. If length(rhs) > 1, will be concatenated with " + " in between
#' @return Returns formula object
#' @export
construct_formula <- function(lhs, rhs, fe=0, clus=0) {
  if (length(rhs) > 1) {
    rhs <- pastep(rhs)
  }
  if (length(fe) > 1) {
    fe <- pastep(fe)
  }
  if (length(clus) > 1) {
    clus <- pastep(clus)
  }
  as.formula(sprintf("%s ~ %s | %s | 0 | %s", lhs, rhs, fe, clus))
}