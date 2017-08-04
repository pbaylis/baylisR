#' Concatenate and flattens strings with " + "
#' @param ... Character strings to concatenate with " + "
#' @return Returns character vector with " + " in between each
#' @export
pastep <- function(...) {
  paste0(..., collapse=" + ")
}
