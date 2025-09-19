#' c_shear
#'
#' Create a c_shear object
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param ws_signals character, name(s) of wind speed signal(s). If NULL, the main\cr
#' signal is used.
#'
#' @importFrom magrittr %>%
#'
#' @export
c_shear <- function(cx = NULL, ws_signals = NULL) {
  # check if cx is c_mseries object
  if (!is.c_mseries(cx))  {
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


  # if (ws2_h < ws1_h) {
  #   ws_signals <- c(ws_signals[2], ws_signals[1])
  #   ws1_h <- as.numeric(cx$wind_speed[ws_signals[1]])
  #   ws2_h <- as.numeric(cx$wind_speed[ws_signals[2]])
  #   ws1_n <- names(cx$wind_speed[ws_signals[1]])
  #   ws2_n <- names(cx$wind_speed[ws_signals[2]])
  #   ws1_data <- as.numeric(cx$mdata[, ws1_n])
  #   ws2_data <- as.numeric(cx$mdata[, ws2_n])
  # }

  # get information about main direction signal
  main_dir_n <- as.character(cx$main_wind_dir)
  main_dir_h <- as.numeric(cx$wind_dir[cx$main_wind_dir])
  main_dir_data <- as.numeric(cx$mdata[, main_dir_n])

  # create a data set for calculations
  shear_input <- data.frame(ws1_data, ws2_data, main_dir_data) %>%
    dplyr::filter(complete.cases(.))

  # dout <- calculate_shear(shear_input$ws1_data, shear_input$ws2_data,
  #                         shear_input$main_dir_data,
  #                         ws1_h, ws2_h)

  # convert 360 to 0
  #shear_input$main_dir_data[which(shear_input$main_dir_data == 360)] <- 0

  shear_out <- rep(0,16)
  n <- as.integer(length(shear_input$ws1_data))
  resp <- .C("calculate_shear_c", n = as.integer(n),
             ws1 = as.double(shear_input$ws1_data), ws2 = as.double(shear_input$ws2_data),
             dir = as.double(shear_input$main_dir_data),
             hl = as.double(ws1_h), hh= as.double(ws2_h),
             shear = as.double(shear_out))
  dout <- resp$shear

  # # create breaks
  # breaks <- seq(0, 360, 15)
  #
  # # get data for calculating shear
  # # shear_data <- dplyr::as_tibble(cx$mdata[, c(ws_signals, main_dir_n)]) %>%
  # #   dplyr::mutate(alpha = calculate_alpha(as.numeric(cx$mdata[,ws1_n]),
  # #                                         as.numeric(cx$mdata[,ws2_n]),
  # #                                         ws1_h, ws2_h)) %>%
  # #   dplyr::mutate(dir_cut = cut(as.numeric(cx$mdata[, main_dir_n]),
  # #                               breaks = c(breaks, 375),
  # #                               ordered_result = TRUE,
  # #                               right = FALSE)) %>%
  # #   dplyr::group_by(dir_cut) %>%
  # #   dplyr::summarise(alpha_mean = mean(alpha))
  # shear_data <- shear_input %>%
  #   dplyr::mutate(alpha = calculate_alpha(ws1_data,
  #                                         ws2_data,
  #                                         ws1_h, ws2_h)) %>%
  #   dplyr::mutate(dir_cut = cut(main_dir_data,
  #                               breaks = c(breaks, 375),
  #                               ordered_result = TRUE,
  #                               right = FALSE)) %>%
  #   dplyr::group_by(dir_cut) %>%
  #   dplyr::summarise(alpha_mean = mean(alpha))
  #
  #
  # #shear_data_binned <- cut(shear_data[, main_dir], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
  # #print(shear_data)
  # dout <- data.frame(dir = seq(0,330, 30), alpha = 0)
  # dout$alpha[1] <- mean(shear_data$alpha_mean[1], shear_data$alpha_mean[24])
  # i <- 2
  # for (iter in seq(2, 23, 2)) {
  #   dout$alpha[i] <- mean(shear_data$alpha_mean[iter], shear_data$alpha_mean[iter+1])
  #   i <- i + 1
  # }

  return(dout)
}
