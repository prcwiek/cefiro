#' c_shear
#'
#' Create a c_shear object
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param ws_signals character, name(s) of wind speed signal(s). If NULL, the main\cr
#' signal is used.
#' @param numeric_directions logical, if TRUE directions as numbers,\cr
#' otherwise as directions names
#'
#' @importFrom magrittr %>%
#'
#' @export
c_shear <- function(cx = NULL, ws_signals = NULL, numeric_directions = TRUE) {
  # check if cx is c_mseries object
  if (!is_c_mseries(cx))  {
    stop("cefiro package error: Invalid input format! Argument is not c_mseries object or a vector.",
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

  ws1_n <- names(cx$wind_speed[ws_signals[1]])
  ws2_n <- names(cx$wind_speed[ws_signals[2]])
  ws1_data <- as.numeric(cx$mdata[, ws1_n])
  ws2_data <- as.numeric(cx$mdata[, ws2_n])

  # get information about main direction signal
  main_dir_n <- as.character(cx$main_wind_dir)
  main_dir_h <- as.numeric(cx$wind_dir[cx$main_wind_dir])
  main_dir_data <- as.numeric(cx$mdata[, main_dir_n])

  # create a data set for calculations
  shear_input <- data.frame(ws1_data, ws2_data, main_dir_data) %>%
    dplyr::filter(complete.cases(.))


  # initialize input vectors for .C call
  count_in <- rep(0,16)   # count of records in each of 16 sectors, from 0 to 337.5
  shear_in <- rep(0,16)   # average shear values in each secotr
  wsl_in <- rep(0,16)     # average wind speed values at  the lower level in each sector
  wsh_in <- rep(0,16)     # average wind speed values at  the lower level in each sector

  # find size of vector to pass to Fortran
  n <- as.integer(length(shear_input$ws1_data))

  resp <- .C("calculate_shear_c", n = as.integer(n),
             ws1 = as.double(shear_input$ws1_data), ws2 = as.double(shear_input$ws2_data),
             dir = as.double(shear_input$main_dir_data),
             hl = as.double(ws1_h), hh = as.double(ws2_h),
             count = as.double(count_in),
             wsl = as.double(wsl_in),
             wsh = as.double(wsh_in),
             shear = as.double(shear_in))

  dout <- tibble::tibble(sector = round(seq(from = 0, to = 337.5, by = 22.5) ,1),
                         count = round(resp$count, 0),
                         ws1 = round(resp$wsl, 3),
                         ws2 = round(resp$wsh, 3),
                         shear = resp$shear)

  # rename wind speed signals names
  names(dout)[3] <- ws1_n
  names(dout)[4] <- ws2_n

  if (!numeric_directions) {
    directions_names <- c("N", "NNE", "NE", "ENE",
                          "E", "ESE", "SE", "SSE",
                          "S", "SSW", "SW", "WSW",
                          "W", "WNW", "NW", "NNW")
    dout$sector <- directions_names
  }

  return(dout)
}
