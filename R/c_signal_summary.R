#' Summary of one measurement signal
#'
#' @description
#' Function presents a summary of one signal in a table.
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param signal character, name of signal. If NULL, the main\cr
#' signal is used.
#' @param start_date character; start date in the format YYYY-MM-DD.
#' @param end_date character; end date in the format YYYY-MM-DD.
#' @param numeric_months logical, if TRUE it returns months as numbers,\cr
#' otherwise as months names
#'
#' @return Function returns a tibble with a signal summary.
#'
#' @export
c_signal_summary <- function(cx = NULL, signal = NULL,
                             start_date = NULL, end_date = NULL,
                             numeric_months = TRUE) {

  # check if cx is c_mseries object
  if (!is_c_mseries(cx))  {
    stop("cefiro package error: Invalid input format! Argument is not c_mseries object.",
         call. = FALSE)
  }

  # check if more than one signal is provided
  if (!is.null(signal) & length(signal) > 1) {
    stop("cefiro package error: Invalid input! Only one signal possible.",
         call. = FALSE)
  }

  # check if a start date and an end date are in the period range of cx
  if (!is.null(start_date)) {
    if (as.POSIXct(start_date, tz = "UTC") < cx$start_date |
        as.POSIXct(start_date, tz = "UTC") > cx$end_date) {
      stop("cefiro package error: Invalid input! Start date out of range.",
           call. = FALSE)
    }
  }
  if (!is.null(end_date)) {
    if (as.POSIXct(end_date, tz = "UTC") > cx$end_date |
        as.POSIXct(end_date, tz = "UTC") < cx$start_date) {
      stop("cefiro package error: Invalid input! End date out of range.",
           call. = FALSE)
    }
  }

  # get all signals names for calculating coverage
  signals_names <- c(names(cx$wind_speed), names(cx$wind_dir), names(cx$temp), names(cx$pressure))

  # get all signals names or check if selected signals exist
  if (is.null(signal)) {
    signal <- cx$main_wind_speed
  } else if (sum(signal %in% signals_names) != length(signal)) {
    stop("cefiro package error: Invalid input! Signal does not exist in c_mseries object.",
         call. = FALSE)
  }

  # filter period
  if (is.null(start_date) & is.null(end_date)) {
    cx <- cx$mdata
  } else if(!is.null(start_date) & is.null(end_date)) {
    cx <- cx$mdata[paste0(start_date, "/")]
  } else if(is.null(start_date) & !is.null(end_date)) {
    cx <- cx$mdata[paste0("/", end_date)]
  } else if (!is.null(start_date) & !is.null(end_date)) {
    cx <- cx$mdata[paste(start_date, end_date, sep = "/")]
  }

  t_index_temp <- as.POSIXlt(zoo::index(cx), tz = "UTC")
  input_year <- lubridate::year(t_index_temp)
  input_month <- lubridate::month(t_index_temp)
  year_month <- paste(input_year, input_month, sep = "_")

  n <- length(year_month)
  unique_year_month <- unique(year_month)
  n_unique_year_month <- length(unique_year_month)

  dout <- tibble::tibble(year = unique(year_month),
                         month = unique(year_month))

  input_signal <- as.double(cx[, signal])
  input_signal <- replace(input_signal, is.na(input_signal), -7777)

  all_records <- rep(0, n_unique_year_month)
  valid_records <- all_records
  coverage <- all_records
  mean_values <- all_records
  min_values <- rep(7777, n_unique_year_month)
  max_values <- all_records

  resp <- .C("signal_summary_c",
             n = as.integer(n), n_unique_year_month = as.integer(n_unique_year_month),
             year = as.integer(input_year), month = as.integer(input_month),
             signal = as.double(input_signal),
             all_records = as.double(all_records),
             valid_records = as.double(valid_records),
             coverage = as.double(coverage),
             mean_values = as.double(mean_values),
             min_values = as.double(min_values),
             max_values = as.double(max_values))

  dout <- dout %>%
    dplyr::bind_cols(all_records = resp$all_records,
                     valid_records = resp$valid_records,
                     coverage = round(resp$coverage, 4),
                     mean = round(resp$mean_values, 3),
                     min = round(resp$min_values, 3),
                     max = round(resp$max_values, 3))

  # Adjust years and months
  year_month_split <-stringr::str_split(dout$year, pattern = "_", simplify = TRUE)
  dout$year <- year_month_split[,1]
  dout$month <- paste0(sub("_", "-", dout$month), "-01")
  if (numeric_months) {
    dout$month <- year_month_split[,2]
  } else {
    dout$month <- lubridate::month(dout$month, label = TRUE, locale = "en_IN")
  }

  return(dout)
}

