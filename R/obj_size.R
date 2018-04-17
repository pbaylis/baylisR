#' Return data.table with lag
#' @param x Name of object 
#' @return Prints string with x's size, units chosen automatically
#' @import data.table
#' @export
obj_size <- function(x) {
  print(object.size(x), units="auto")
}

