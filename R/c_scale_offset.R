#' Scale and offset a signal
#'
#' @description
#' Scale and offset a provided signal
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param signal character, a name of a wind speed signal. If NULL, the main\cr
#' signal is used.
#' @param scale numeric, a value of a scale parameter
#' @param offset numeric, a value of a offset parameter
#'
#' @importFrom magrittr %>%
#'
#' @return c_mseries object with a provided signal multiply by scale and with\cr
#' added offset
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
c_scale_offset <- function(cx = NULL, signal = NULL,
                           scale = NULL, offset = NULL) {
  # check if cx is c_mseries object
  if (!is_c_mseries(cx))  {
    stop("cefiro package error: Invalid input format! Argument is not c_mseries object.",
         call. = FALSE)
  }
  # check if ws_signalsis NULL
  if (is.null(signal)) {
    stop("cefiro package error: Invalid input format! Argument signals is not provided.",
         call. = FALSE)
  }

  # check if exactly two ws_signals are provided
  if (length(signal) > 1) {
    stop("cefiro package error: Invalid input! Too many wind speed signals provided.",
         call. = FALSE)
  }

  # get all signals names for calculating coverage
  if (!(signal %in% names(cx$mdata))) {
    stop("cefiro package error: Invalid input format! Provided signal does not exist.",
         call. = FALSE)
  }

  # check if scale and offset are provided
  if (is.null(scale) | is.null(offset)) {
    stop("cefiro package error: Invalid input! Scale and offset are incorrect.",
         call. = FALSE)
  }

  # check if scale and offset are numeric
  if (!is.numeric(scale) | !is.numeric(offset)) {
    stop("cefiro package error: Invalid input! Scale and offset are not numeric.",
         call. = FALSE)
  }

  # get signal data
  signal_data <- as.numeric(cx$mdata[, signal])

  # scale and offset
  signal_data <- signal_data * scale + offset

  cx$mdata[, signal] <- signal_data

  cx

}
