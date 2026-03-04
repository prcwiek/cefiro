#' Wind speed extrapolation with directional shear
#'
#' @description
#' Create an extrapolated wind speed series with provided\cr
#' shear values for 16 sectors.
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param ws_signal character; name of wind speed signal to extrapolate.
#' @param dir_signal character; name of direction signal to use.
#' @param ws_min numeric; minimal wind speed at the lower sensors.
#' @param dir_shear numeric vector with shear values for 16 sectors.
#' @param height numeric; height to extrapolate.
#' @param start_date character; start date in the format YYYY-MM-DD.
#' @param end_date character; end date in the format YYYY-MM-DD.
#'
#' @return Function returns an extrapolated c_mseries wind speed series
#'
#' @importFrom magrittr %>%
#'
#' @export
c_extrapolate_dir_shear <- function(cx = NULL, ws_signal = NULL, dir_signal = NULL,
                                    ws_min = NULL,
                                    dir_shear = NULL,
                                    height = NULL,
                                    start_date = NULL, end_date = NULL) {
  # check if cx is c_mseries object
  if (!is_c_mseries(cx))  {
    stop("cefiro package error: Invalid input format! Argument is not c_mseries object.",
         call. = FALSE)
  }
  # check if ws_signal is NULL
  if (is.null(ws_signal)) {
    stop("cefiro package error: Invalid input format! Argument ws_signal is not provided.",
         call. = FALSE)
  }
  # check if dir_signal is NULL
  if (is.null(dir_signal)) {
    stop("cefiro package error: Invalid input format! Argument dir_signal is not provided.",
         call. = FALSE)
  }
  # check if exactly only one ws_signal is provided
  if (length(ws_signal) > 1) {
    stop("cefiro package error: Invalid input! Too many wind speed signals provided.",
         call. = FALSE)
  }
  # check if exactly only one dir_signal is provided
  if (length(dir_signal) > 1) {
    stop("cefiro package error: Invalid input! Too many direction signals provided.",
         call. = FALSE)
  }
  # check if values for directional shear are provided
  if (is.null(dir_shear) | length(dir_shear) != 16 | !is.numeric(dir_shear)  ) {
    stop("cefiro package error: Invalid input! Directional shear vector is incorrect.",
         call. = FALSE)
  }
  # check if height value provided
  if (is.null(height) | height <= 0 | !is.numeric(dir_shear)  ) {
    stop("cefiro package error: Invalid input! Height to extrapolate is incorrect.",
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

  # get signals names
  ws_n <- names(cx$wind_speed[ws_signal])
  dir_n <- names(cx$wind_dir[dir_signal])

  # get signals heights
  ws_h <- as.numeric(cx$wind_speed[ws_signal])
  dir_h <- as.numeric(cx$wind_dir[dir_signal])

  # get series name
  series_n <- cx$name

  # get series time zone
  tzone_n <- cx$tzone

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

  # get wind speed  and dir signals data
  ws_data <- as.numeric(cx[, ws_n])
  dir_data <- as.numeric(cx[, dir_n])

  # create a data set for calculations and replace NA by 7777
  # wind speeds in these rows will not be extrapolated
  extrapolation_input <- data.frame(ws_data, dir_data) %>%
    tidyr::replace_na(list(ws_data = 7777, dir_data = 7777))


  # find size of vector to pass to Fortran
  n <- as.integer(length(extrapolation_input$ws_data))

  # initialize input vectors for .C call
  ws_extrapolated_in <- rep(0, n)

  resp <- .C("extrapolate_dir_shear_c",
             n = as.integer(n),
             ws = as.double(extrapolation_input$ws_data),
             dir = as.double(extrapolation_input$dir_data),
             shear = as.double(dir_shear),
             hl = ws_h,
             he = as.double(height),
             ws_extrapolated = as.double(ws_extrapolated_in))

  # replace 7777 by NA
  ws_extrapolated <- dplyr::replace_values(resp$ws_extrapolated, from = 7777, to = NA)
  dir_signal <- dplyr::replace_values(resp$dir, from = 7777, to = NA)

  dout <- tibble::tibble(DateTime = zoo::index(cx),
                         ws_extrapolated = ws_extrapolated,
                         dir = dir_signal)
  names(dout)[3] <- dir_n

  dout <- cefiro::c_mseries(dout,
                            date_col = "DateTime",
                            ws_col = "ws_extrapolated",
                            ws_h = height,
                            dir_col = dir_n,
                            dir_h = dir_h,
                            name = paste0(series_n, "_extrapolated"),
                            tzone = tzone_n)

  dout

}
