#' c_coverage
#'
#' Function presents coverage of wind measuremetn data.
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param signals character, name(s) of signal(s). If NULL, the main\cr
#' signal is used.
#'
#' @export
c_coverage <- function(cx = NULL, signals = NULL) {
  # check if cx is c_mseries object
  if (!is.c_mseries(cx))  {
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

  # get time index
  t_index <- as.POSIXlt(zoo::index(cx$mdata), tz = cx$tzone)
  # extract xts object
  dx <- cx$mdata
  # create year & month columns
  dx$year <- lubridate::year(t_index)
  dx$month <- lubridate::month(t_index)

  # calculate monthly coverage
  dout <- as.data.frame(dx) %>%
    dplyr::group_by(year, month) %>%
    dplyr::summarise(dplyr::across(.cols = tidyselect::all_of(signals), ~(1-sum(is.na(.x))/length(.x)))) %>%
    dplyr::mutate(across(.cols = tidyselect::all_of(signals), ~round(.x, 4))) %>%
    dplyr::mutate(month = lubridate::month(month, label = TRUE, locale = "en_IN")) %>%
    dplyr::ungroup()

  dout
}


