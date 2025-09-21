#' is_c_mseries
#'
#' Test if the object is a c_mseries
#'
#' This function returns `TRUE` for c_mseries,
#' and `FALSE` for all other objects.
#'
#' @param x An object
#'
#' @return `TRUE` if the object inherits from the `c_mseries` class.
#'
#' @export
is_c_mseries <- function(x) {
  inherits(x, "c_mseries")
}
