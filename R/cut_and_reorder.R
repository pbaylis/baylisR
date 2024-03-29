#' Reorder a variable quickly for a regression.
#' @param x Numeric vector.
#' @param cut.seq (Optional) Numeric vector to pass to cut
#' @param omit (Optional) Bin with this number will be placed first in vector.
#' @return Factorized version of x, with given sequence and omitted level first.
#' @export
cut_and_reorder  <- function(x, cut.seq=c(-Inf, seq(0, 40, 5), Inf), omit=22.5) {
  x.cut <- cut(x, cut.seq)
  x.cut <- relevel(x.cut, findInterval(omit, cut.seq))
  return(x.cut)
}
