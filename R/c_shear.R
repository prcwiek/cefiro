#' Directional wind shear
#'
#' @description
#' Create a table with directional wind shear values for 16 sectors
#' based on two measurement heights.
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param ws_signals character, names of wind speed signals.
#' @param start_date character; start date in the format YYYY-MM-DD.
#' @param end_date character; end date in the format YYYY-MM-DD.
#' @param numeric_directions logical, if TRUE directions as numbers,\cr
#' otherwise as directions names
#'
#' @return Function returns a tibble with wind shares values for 16 directions.
#'
#' @examples
#' \dontrun{
#' c_shear(cx, ws_signals = c("WS77", "WS125"))
#' c_shear(cx, ws_signals = c("WS77", "WS125"), numeric_directions = FALSE)
#' c_shear(cx, ws_signals = c("WS77", "WS125"), start_date = "1998-03-01", end_date = "1998-04-30")
#' c_shear(cx, ws_signals = c("WS77", "WS125"), start_date = "1998-03-01")
#' c_shear(cx, ws_signals = c("WS77", "WS125"), end_date = "1998-04-30")
#' }
#'
#' @importFrom magrittr %>%
#'
#' @export
c_shear <- function(cx = NULL, ws_signals = NULL,
                    start_date = NULL, end_date = NULL,
                    numeric_directions = TRUE) {
  # check if cx is c_mseries object
  if (!is_c_mseries(cx))  {
    stop("cefiro package error: Invalid input format! Argument is not c_mseries object.",
         call. = FALSE)
  }
  # check if ws_signals si NULL
  if (is.null(ws_signals)) {
    stop("cefiro package error: Invalid input format! Argument signals is not provided.",
         call. = FALSE)
  }
  # check if exactly two ws_signals are provided
  if (length(ws_signals) > 2) {
    stop("cefiro package error: Invalid input! Too many wind speed signals provided.",
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

  # define shear function\
  calculate_alpha <- function(w_low, w_high, h_low, h_high) {
    log(w_high/w_low) / log(h_high/h_low)
  }

  # get information about provided wind speed signals
  ws1_h <- as.numeric(cx$wind_speed[ws_signals[1]])
  ws2_h <- as.numeric(cx$wind_speed[ws_signals[2]])

  # check heights and switch if necessary
  if (ws1_h > ws2_h) {
    ws_temp <- ws1_h
    ws1_h <- ws2_h
    ws2_h <- ws_temp
    rm(ws_temp)
  }

  # getting signlas names
  ws1_n <- names(cx$wind_speed[ws_signals[1]])
  ws2_n <- names(cx$wind_speed[ws_signals[2]])
  main_dir_n <- as.character(cx$main_wind_dir)
  main_dir_h <- as.numeric(cx$wind_dir[cx$main_wind_dir])

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

  # get wind spped signals data
  ws1_data <- as.numeric(cx[, ws1_n])
  ws2_data <- as.numeric(cx[, ws2_n])

  # get main direction signal data
  main_dir_data <- as.numeric(cx[, main_dir_n])

  # create a data set for calculations
  shear_input <- data.frame(ws1_data, ws2_data, main_dir_data) %>%
    dplyr::filter(complete.cases(.))


  # initialize input vectors for .C call
  records_in <- rep(0,16)   # count of records in each of 16 sectors, from 0 to 337.5
  shear_in <- rep(0,16)   # average shear values in each secotr
  wsl_in <- rep(0,16)     # average wind speed values at  the lower level in each sector
  wsh_in <- rep(0,16)     # average wind speed values at  the lower level in each sector

  # find size of vector to pass to Fortran
  n <- as.integer(length(shear_input$ws1_data))

  resp <- .C("calculate_shear_c", n = as.integer(n),
             ws1 = as.double(shear_input$ws1_data), ws2 = as.double(shear_input$ws2_data),
             dir = as.double(shear_input$main_dir_data),
             hl = as.double(ws1_h), hh = as.double(ws2_h),
             records = as.double(records_in),
             wsl = as.double(wsl_in),
             wsh = as.double(wsh_in),
             shear = as.double(shear_in))

  dout <- tibble::tibble(sector = round(seq(from = 0, to = 337.5, by = 22.5) ,1),
                         records = round(resp$records, 0),
                         ws1 = round(resp$wsl, 3),
                         ws2 = round(resp$wsh, 3),
                         shear = resp$shear)

  # rename wind speed signals names
  names(dout)[3] <- ws1_n
  names(dout)[4] <- ws2_n

  # Adjust directions names
  if (!numeric_directions) {
    directions_names <- c("N", "NNE", "NE", "ENE",
                          "E", "ESE", "SE", "SSE",
                          "S", "SSW", "SW", "WSW",
                          "W", "WNW", "NW", "NNW")
    dout$sector <- directions_names
  }

  return(dout)
}
