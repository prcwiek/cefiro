#' c_means
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param mom logical, if TRUE, means of monthly means will be calculated
#' #' #' @param signals character, name(s) of signal(s). If NULL, the main\cr
#' signal is used.
#'
#' @export
c_means <- function(cx = NULL, mom = FALSE, signals = NULL) {
  # check if cx is c_mseries object
  if (!is.null(cx) & !is.c_mseries(cx))  {
    stop("cefiro package error: Invalid input format! Argument is not c_mseries object or a vector.", call. = FALSE)
  }

  # get all signals names for calculating coverage
  signals_names <- c(names(cx$wind_speed), names(cx$wind_dir), names(cx$temp), names(cx$pressure))

  # get all signals names or check if selected signals exist
  if (is.null(signals)) {
    signals <- signals_names
  } else if (sum(signals %in% signals_names) != length(signals)) {
    stop("cefiro package error: Invalid input! One of signals does not exist in c_mseries object.",
         call. = FALSE)
  }

  # signals <- c(names(cx$wind_speed), names(cx$wind_dir),
  #              names(cx$temp), names(cx$pressure))
  # signals <- signals[!is.null(signals)]

  # get time index
  #t_index <- as.POSIXlt(zoo::index(cx$mdata), origin = "1970-01-01", tz = cx$tzone)
  t_index <- as.POSIXlt(zoo::index(cx$mdata), tz = cx$tzone)
  # extract xts object
  dx <- cx$mdata
  # creat year & month columns
  dx$year <- lubridate::year(t_index)
  dx$month <- lubridate::month(t_index)
  # calucate monthly coverages

  dout <- as.data.frame(dx) %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(dplyr::across(.cols = signals, ~mean(.x, na.rm = TRUE))) %>%
    dplyr::mutate(across(.cols = signals, ~round(.x, 2))) %>%
    dplyr::mutate(month = lubridate::month(month, label = TRUE, locale = "en_IN")) %>%
    dplyr::ungroup()

  if (mom) {
    dout <- dout %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(dplyr::across(.cols = signals, ~mean(.x, na.rm = TRUE))) %>%
      dplyr::mutate(across(.cols = signals, ~round(.x, 2))) %>%
      #dplyr::mutate(month = lubridate::month(month, label = TRUE, locale = "en_IN")) %>%
      dplyr::ungroup()
  }

  dout[is.na(dout)] <- NA

  return(dout)

}


