#' @description  Cefiro
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
#' @useDynLib cefiro
NULL

#' Example of wind measurement data
#'
#' A data set containing 10 minutes averages from 1998 to 1999
#'
#' From <https://ourairports.com>
#' A data set is in the public domain according to <https://ourairports.com/data/>
#'
#' \itemize{
#'   \item DateTime; time stamp
#'   \item WS125; wind speed at 125 m AGL
#'   \item WS77; wind speed at 77 m AGL
#'   \item WS44; wind speed at 44 m AGL
#'   \item WD77; wind direction at 77 m AGL
#'   \item WD125; wind direction at 125 m AGL
#'   \item T3; temperature at 3 m AGL
#'   \item T44; wind direction at 44 m AGL
#'   \item T118; wind direction at 118 m AGL
#'   \item P8; wind direction at 125 m AGL
#' }
#'
#' @docType data
#' @keywords wind measurement data
#' @name winddata
#' @author Kurt Schaldemose Hansen
#' @author Nikola Vasiljevic
#' @author Steen Arne Sørensen
#' @references [https://doi.org/10.11583/DTU.c.5405286.v4](https://doi.org/10.11583/DTU.c.5405286.v4)
#' @format A data frame with 102816 rows and 10 variables
NULL
