#' Bind columns of two c_mseries objects
#'
#' @description
#' Bind new columns of c_mseries objects
#'
#' @param cx1 c_mseries; primary object
#' @param cx2 c_mseries; object to append
#'
#' @return c_mseries object with data columns form c_mseries input objects
#'
#' @importFrom stats time
#'
#' @export
c_cbind <- function(cx1 = NULL, cx2 = NULL) {
  # check if cx1 and cx2 are c_mseries objects
  if (!is_c_mseries(cx1))  {
    stop("cefiro package error: Invalid input format! First argument is not c_mseries object.",
         call. = FALSE)
  }
  if (!is_c_mseries(cx2))  {
    stop("cefiro package error: Invalid input format! Second argument is not c_mseries object.",
         call. = FALSE)
  }

  # get signals names from cx1
  cx1_names <- names(cx1$mdata)
  cx1_main_wind_speed <- cx1$main_wind_speed
  cx1_main_dir <- cx1$main_wind_dir
  cx1_name <- cx1$name
  cx1_tzone <- cx1$tzone

  # get signals heights from cx1
  cx1_h_wind_speeds <- as.numeric(cx1$wind_speed)
  cx1_h_wind_dir <- as.numeric(cx1$wind_dir)
  cx1_h_temp <- as.numeric(cx1$temp)
  cx1_h_pressure <- as.numeric(cx1$pressure)

  # get signals names from cx2
  cx2_names <- names(cx2$mdata)
  cx2_main_wind_speed <- cx2$main_wind_speed
  cx2_main_dir <- cx2$main_wind_dir
  cx2_name <- cx2$name

  # get signals heights from cx2
  cx2_h_wind_speeds <- as.numeric(cx2$wind_speed)
  cx2_h_wind_dir <- as.numeric(cx2$wind_dir)
  cx2_h_temp <- as.numeric(cx2$temp)
  cx2_h_pressure <- as.numeric(cx2$pressure)

  # get xts data
  cx1_data <- cx1$mdata
  cx2_data <- cx2$mdata

  cx_binded <- xts::cbind.xts(cx1_data, cx2_data, tzone = cx1_tzone)
  rm(cx1_data)
  rm(cx2_data)
  #xts::tformat(cx_binded) <- "%Y-%m-%d %H:%M:%S"
  tt <- time(cx_binded)
  tt <- format(tt, format = "%Y-%m-%d %H:%M:%S")
  #tt <- stringr::str_remove(tt, pattern = " CET")
  #tt <- as.POSIXct(tt, format = "%Y-%m-%d %H:%M:%S")
  tt <- as.POSIXct(format(tt, format = "%Y-%m-%d %H:%M:%S"))
  cx_binded <- cbind(tt, as.data.frame(cx_binded, row.names = FALSE))
  names(cx_binded)[1] <- "DateTime"

  dout <- c_mseries(mdata = cx_binded,
                    date_col = "DateTime",
                    ws_col = c(names(cx1$wind_speed), names(cx2$wind_speed)),
                    ws_h = c(cx1_h_wind_speeds, cx2_h_wind_speeds),
                    ws_sd_col = c(names(cx1$wind_sd), names(cx2$wind_sd)),
                    dir_col = c(names(cx1$wind_dir), names(cx2$wind_dir)),
                    dir_h = c(cx1_h_wind_dir, cx2_h_wind_dir),
                    dir_sd_col = c(names(cx1$dir_sd_col), names(cx2$dir_sd_col)),
                    t_col = c(names(cx1$temp), names(cx2$temp)),
                    t_h = c(cx1_h_temp, cx2_h_temp),
                    p_col = c(names(cx1$pressure), names(cx2$pressure)),
                    p_h = c(cx1_h_pressure, cx2_h_pressure),
                    name = paste0(cx1_name, " - ", cx2_name),
                    tzone = cx1_tzone)

  dout$main_wind_speed <- cx1_main_wind_speed
  dout$main_wind_dir <- cx1_main_dir

  dout

}
