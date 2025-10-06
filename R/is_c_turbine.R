#' is_c_turbine
#'
#' Test if the object is a c_turbine.
#'
#' This function returns `TRUE` for c_turbine,
#' and `FALSE` for all other objects.
#'
#' @param x An object
#'
#' @return `TRUE` if the object inherits from the `c_turbine` class.
#'
#' @export
is_c_turbine <- function(x) {
  inherits(x, "c_turbine")
}
