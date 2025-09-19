#'  c_mseries
#'
#' Create c_mseries object with data and information about wind measurement.
#' The output c_mseries object consists of:
#' \itemize{
#' \item mdata in xts Time-Series Object  with wind measurement data
#' \item t_index with dates
#' \item start_date, a start date of measurement data
#' \item end_date, an end date of measurement data
#' \item wind_speed, names and heights of measured wind speed signals
#' \item wind_dir, names and heights of measured wind direction signals
#' \item wind_sd, names and heights of measured standard deviation
#' \item temp, names and heights of temperature measured signals
#' \item pressure, names and heights of winn speed measured signals
#' \item main_wind_speed, main wind speed measured signal
#' \item wind_dir, main wind direction measured signals
#' }
#'
#' @param mdata data frame; wind measurements data, names of columns are required.
#' @param date_col character; name of column with dates and hours.\cr
#' A selected column should be POSIXct.
#' @param ws_col character; vector with names of columns with wind speed signals.
#' @param ws_h numeric; vector with heights of wind speed signals.
#' @param ws_sd_col character; vector with names of columns with standard deviation of\cr
#' wind speed signals.
#' @param dir_col character; vector with names of columns with wind direction signals.
#' @param dir_h numeric; vector with heights of wind direction signals.
#' @param dir_sd_col character; vector with names of columns with standard deviation of\cr
#' wind direction signals
#' @param t_col character; vector with names of columns with temperature signals.
#' @param t_h numeric; vector with heights of temperature signals.
#' @param p_col character; vector with names of columns with pressure signals.
#' @param p_h  numeric; vector with heights of pressure signals.
#' @param ws_main character; name of main wind speed signal. If NULL, the highest\cr
#' signal will be selected.
#' @param wd_main character; name of main wind direction signal. If NULL, the highest\cr
#' signal will be selected.
#' @param name character; name of measurement site.
#' @param tzone character; tzone = "UTC"
#'
#' @return c_mseries object with measurement data and information about a measurement site
#'
#' @examples
#' \dontrun{
#' # Creating c_mseries object from the din data frame where
#' # the column DateTime consists of the time stamps in
#' the format 1999-01-01 00:10:00:
#' df <- c_mseries(mdata = din, date_col = "DateTime",
#' ws_col = c("WS125", "WS77", "WS44"), dir_col = c("WD77", "WD125"),
#' ws_h = c(125, 77, 44), dir_h = c(77, 125),
#' t_col = c("T3", "T44", "T118"), t_h = c(3, 44, 118),
#' p_col = "P8", p_h = 8,
#' name = "Test mast")
#' }
#'
#' @export
c_mseries <- function(mdata, date_col, ws_col, ws_h, ws_sd_col,
                      dir_col, dir_h, dir_sd_col,
                      t_col, t_h,
                      p_col, p_h,
                      ws_main, wd_main,
                      name, tzone, ...) {
  UseMethod("c_mseries")
}

#' @export
c_mseries.default <- function(mdata = NULL, date_col = NULL,
                              ws_col = NULL, ws_h = NULL,
                              ws_sd_col = NULL,
                              dir_col = NULL, dir_h = NULL,
                              dir_sd_col = NULL,
                              t_col = NULL, t_h = NULL,
                              p_col = NULL, p_h = NULL,
                              ws_main = NULL, wd_main = NULL,
                              name = NULL, tzone = "UTC", ...) {

  # check if mdata is a data frame
  if (!is.data.frame(mdata)) {
    stop("cefiro package error: Parameter mdata is not a data frame!", call. = FALSE)
  }
  # check if date_col is not NULL
  if (is.null(date_col)) {
    stop("cefiro package error: Parameter date_col is NULL. Please select a column!", call. = FALSE)
  }
  # check if date_col column exists and it is POSIXct
  if (date_col %in% names(mdata)) {
    t <- as.data.frame(mdata[1, date_col])
    if (!xts::is.timeBased(t[1,1])) {
      stop(paste0("cefiro package error: date_col ", date_col, " is not a time-based R class"), call. = FALSE)
    }
  } else {
    stop(paste0("cefiro package error: date_col = ", date_col, " does not exist!"), call. = FALSE)
  }

  # create time series
  mdata <- as.data.frame(mdata)
  t_index <-  mdata[, date_col]
  n_index <- names(mdata)[which(names(mdata) != date_col)]
  mdata <- subset(mdata, select = n_index)
  dout <- xts::xts(mdata, order.by = t_index, tzone = tzone)

  # check if a created time series has all records
  # if not then create lines in NA
  t_start = min(t_index)
  t_end = max(t_index)
  t_period = round(as.numeric((t_end - t_start)), 0)*24*6
  check_index <- seq(t_start, t_end, 600)
  dcheck <- xts::xts(c(1:t_period), order.by = check_index, tzone = tzone)
  dout <- cbind(dout, dcheck)
  dout <- dout[, n_index]

  wind_speed <- as.list(setNames(ws_h, ws_col))
  wind_dir <- as.list(setNames(dir_h, dir_col))
  if (is.null(ws_sd_col)) {
    wind_sd <- NULL
  } else {
    wind_sd <- as.list(setNames(ws_h, ws_sd_col))
  }


  if (is.null(ws_main)) {
    main_wind_speed <- names(which.max(wind_speed))
  } else {
    main_wind_speed <- ws_main
  }
  if (is.null(wd_main)) {
    main_wind_dir <- names(which.max(wind_dir))
  } else {
    main_wind_dir <- wd_main
  }

  structure(list(mdata = dout, t_index = t_index,
                 start_date = t_start, end_date = t_end,
                 wind_speed = wind_speed,
                 wind_dir = wind_dir,
                 wind_sd = wind_sd,
                 temp = as.list(setNames(t_h, t_col)),
                 pressure = as.list(setNames(p_h, p_col)),
                 main_wind_speed = main_wind_speed, main_wind_dir = main_wind_dir,
                 name = name,
                 tzone = tzone), class = "c_mseries")
}

#' @export
is.c_mseries <- function(object) {
  inherits(object, "c_mseries")
}

#' @export
print.c_mseries <- function(x, ...) {
  if (!is.c_mseries(x)) {
    stop("cefiro package error: Invalid input format! Argument is not a m_series x.", call. = FALSE)
  }
  cat("Measurement time series: ", x$name," \n")
  #cat(sprintf("Start date: %s \n", as.character(x$start_date, format = "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("Start date: %s \n", as.character(format(x$start_date, format = "%Y-%m-%d %H:%M:%S"))))
  #cat(sprintf("End date: %s \n", as.character(x$end_date, format = "%Y-%m-%d %H:%M:%S")))
  cat(sprintf("End date: %s \n", as.character(format(x$end_date, format = "%Y-%m-%d %H:%M:%S"))))
  cat("------------------------------\n")
  cat(sprintf("Wind speed signal %s measured at height: %.2f\n", names(x$wind_speed), unlist(unname(x$wind_speed))),
      sep = "")
  cat(sprintf("Wind direction signal %s measured at height: %.2f\n", names(x$wind_dir), unlist(unname(x$wind_dir))),
      sep = "")
  cat(sprintf("Temperature signal %s measured at height: %.2f\n", names(x$temp), unlist(unname(x$temp))),
      sep = "")
  cat(sprintf("Pressure signal %s measured at height: %.2f\n", names(x$pressure), unlist(unname(x$pressure))),
      sep = "")
  cat("------------------------------\n")
  cat("Main wind speed signal ", x$main_wind_speed, "\n")
  cat("Main wind direction signal ", x$main_wind_dir, "\n")
  cat("\n")
}

#' @export
hist.c_mseries <- function(x, signal = NULL, col = "blue", freq = FALSE,
                           start_date = NULL, end_date = NULL,
                           vmin = 0, vmax = 30, ...) {
  if (!is.c_mseries(x)) {
    stop("cefiro package error: Invalid input format! Argument is not a m_series x.", call. = FALSE)
  }

  if (length(signal) > 1) {
    stop("cefiro package error: Only one signal for histogram!", call. = FALSE)
  }

  # check start date
  if (!is.null(start_date)) {
    if (start_date < min(x$t_index) | start_date > max(x$t_index)) {
      stop("cefiro package error: Invalid input! Start date out of scope.",
           call. = FALSE)
    }
  }
  if (!is.null(end_date)) {
    if (end_date > max(x$t_index) | end_date < min(x$t_index)) {
      stop("cefiro package error: Invalid input! End date out of scope.",
           call. = FALSE)
    }
  }
  # define start and en dates if not provided in inputs
  if (is.null(start_date)) {
    start_date <- min(x$t_index)
  }
  if (is.null(end_date)) {
    end_date <- max(x$t_index)
  }

  # trim data according start and end dates
  slicer_date <- paste0(start_date, "/", end_date)

  signals_names <- c(names(x$wind_speed), names(x$wind_dir), names(x$wind_sd), names(x$temp),
                     names(x$pressure))
  if (sum(signal %in% signals_names) != length(signal)) {
    stop("cefiro package error: Invalid input! One of signals does not exist in c_mseries x.",
         call. = FALSE)
  }
  if (is.null(signal)) {
    dfhist <- xts::as.xts(x$mdata[slicer_date, x$main_wind_speed])
    signal <- as.character(x$main_wind_speed)
    h <- as.numeric(x$wind_speed[signal])
    breaks <- c(vmin:vmax)
    dfhist <- dfhist[dfhist[,signal] >= vmin & dfhist[,signal] <= vmax]
    signal_name <- ""
    units_name <- " m/s"
  } else {
    dfhist <- xts::as.xts(x$mdata[slicer_date, signal])
    signal <- as.character(signal)
    if (!is.na(names(x$wind_speed[signal]))) {
      h <- as.numeric(x$wind_speed[signal])
      breaks = c(vmin:vmax)
      dfhist <- dfhist[dfhist[,signal] >= vmin & dfhist[,signal] <= vmax]
      signal_name <- "Wind speed "
      units_name <- " m/s"
    } else if (!is.na(names(x$wind_dir[signal]))) {
      h <- as.numeric(x$wind_dir[signal])
      breaks = seq(0, 360, 30)
      dfhist <- dfhist[dfhist[,signal] >= 0 & dfhist[,signal] < 360]
      signal_name <- "Wind direction "
      units_name <- " deg"
    } else if (!is.na(names(x$temp[signal]))) {
      h <- as.numeric(x$temp[signal])
      breaks = seq(-50, 50, 5)
      dfhist <- dfhist[dfhist[,signal] >= -50 & dfhist[,signal] <= 50]
      signal_name <- "Temperature "
      units_name <- " C"
    } else if (!is.na(names(x$pressure[signal]))) {
      h <- as.numeric(x$pressure[signal])
      breaks = seq(950, 1050, 10)
      dfhist <- dfhist[dfhist[,signal] >= 950 & dfhist[,signal] <= 1050]
      signal_name <- "Pressure "
      units_name <- " hPa"
    }
  }

  if (freq) t_ylab <- "Count" else t_ylab <- "Probability"

  hist(dfhist,
       freq = freq,
       breaks = breaks,
       main = paste0("Measurements: ", x$name),
       xlab = paste0(signal_name, signal, units_name, " at ", h, " m"),
       ylab = t_ylab,
       col = col,
       ...)
}

#' @export
plot.c_mseries <- function(x, signal = NULL, col = "blue", lty = "solid",
                           start_date = NULL, end_date = NULL,
                           legend.loc = "topright", ylim = NULL, ...) {
  if (!is.c_mseries(x)) {
    stop("cefiro package error: Invalid input format! Argument is not a c_mseries x.",
         call. = FALSE)
  }
  signals_names <- c(names(x$wind_speed), names(x$wind_dir), names(x$wind_sd), names(x$temp),
                     names(x$pressure))
  if (sum(signal %in% signals_names) != length(signal)) {
    stop("cefiro package error: Invalid input! One of signals does not exist in c_mseries x.",
         call. = FALSE)
  }

  # check start date
  if (!is.null(start_date)) {
    if (start_date < min(x$t_index) | start_date > max(x$t_index)) {
      stop("cefiro package error: Invalid input! Start date out of scope.",
           call. = FALSE)
    }
  }
  if (!is.null(end_date)) {
    if (end_date > max(x$t_index) | end_date < min(x$t_index)) {
      stop("cefiro package error: Invalid input! End date out of scope.",
           call. = FALSE)
    }
  }
  # define start and en dates if not provided in inputs
  if (is.null(start_date)) {
    start_date <- min(x$t_index)
  }
  if (is.null(end_date)) {
    end_date <- max(x$t_index)
  }

  # trim data according start and end dates
  slicer_date <- paste0(start_date, "/", end_date)

  if (is.null(signal)) {
    dfplot <- xts::as.xts(x$mdata[slicer_date, x$main_wind_speed])
    signal <- as.character(x$main_wind_speed)
    h <- as.numeric(x$wind_speed[signal])
    signal_name <- "Wind speed "
  } else {
    dfplot <- xts::as.xts(x$mdata[slicer_date, signal])
    signal <- as.character(signal)
    if (sum(!is.na(names(x$wind_speed[signal]))) == length(names(x$wind_speed[signal]))) {
      h <- as.numeric(x$wind_speed[signal])
      signal_name <- "Wind speed "
    } else if (sum(!is.na(names(x$wind_dir[signal]))) == length(names(x$wind_dir[signal]))) {
      h <- as.numeric(x$wind_dir[signal])
      signal_name <- "Wind direction "
    } else if (sum(!is.na(names(x$temp[signal]))) == length(names(x$temp[signal]))) {
      h <- as.numeric(x$temp[signal])
      signal_name <- "Temperature "
    } else if (sum(!is.na(names(x$pressure[signal]))) == length(names(x$pressure[signal]))) {
      h <- as.numeric(x$pressure[signal])
      signal_name <- "Pressure "
    } else {
      h <- NULL
    }
  }

  if (is.null(h)) {
    main_title <- paste0(signal)
  } else {
    main_title <- paste0(signal_name, signal, " at ", h, " m")
  }

  if (length(main_title) > 1) {
    main_title <- stringr::str_c(main_title, collapse = ", ")
  } else if (is.null(h)) {
    main_title <- stringr::str_c(main_title, collapse = ", ")
    main_title <- paste0("Signals ", main_title)
  }

  #cefiro_palette <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#56B4E9" , "#D55E00", "#CC79A7")
  if (sum(length(col) != length(names(x$wind_speed[signal])))) {
    col <- c("#0072B2", "#E69F00", "#009E73", "#F0E442", "#56B4E9" , "#D55E00", "#CC79A7")
  }
  if (sum(signal %in% names(x$wind_dir) > 0 & is.null(ylim)) ) {
    ylim = c(0, 359)
  }
  xts::plot.xts(x = dfplot, main = main_title, lty = lty, col = col, legend.loc = legend.loc,
                ylim = ylim, ...)
}

#' @export
mean.c_mseries <- function(x, signal = NULL, ...) {
  if (is.null(signal)) {
    mean(x$mdata[,x$main_wind_speed], na.rm = TRUE)
  } else {
    mean(x$mdata[,signal], na.rm = TRUE)
  }
}

#' @export
median.c_mseries <- function(x, na.rm = FALSE, signal = NULL, ...) {
  if (is.null(signal)) {
    median(x$mdata[,x$main_wind_speed], na.rm = TRUE)
  } else {
    median(x$mdata[,signal], na.rm = TRUE)
  }
}

#' @export
max.c_mseries <- function(x, signal = NULL, ...) {
  if (is.null(signal)) {
    max(x$mdata[,x$main_wind_speed], na.rm = TRUE)
  } else {
    max(x$mdata[,signal], na.rm = TRUE)
  }
}

#' @export
min.c_mseries <- function(x, signal = NULL, ...) {
  if (is.null(signal)) {
    min(x$mdata[,x$main_wind_speed], na.rm = TRUE)
  } else {
    min(x$mdata[,signal], na.rm = TRUE)
  }
}

#' @export
summary.c_mseries <- function(object, ...) {
  if (!is.c_mseries(object)) {
    stop("cefiro package error: Invalid input format! Argument is not a m_series object.", call. = FALSE)
  }
  cat("Measurement time series: ", object$name," \n")
  cat(sprintf("Start date: %s \n", as.character(format(object$start_date, format = "%Y-%m-%d %H:%M:%S"))))
  cat(sprintf("End date: %s \n", as.character(format(object$end_date, format = "%Y-%m-%d %H:%M:%S"))))
  for (item in names(object$wind_speed)) {
    cat(sprintf("Wind speed %s: mean %.2f m/s, median %.2f m/s, min %.2f m/s, max %.2f m/s\n",
                item,
                mean(object, signal = item),
                median(object, signal = item),
                min(object, signal = item),
                max(object, signal = item)),
        sep = "")
  }
  for (item in names(object$wind_sd)) {
    cat(sprintf("SD wind speed %s: mean %.2f m/s, median %.2f m/s, min %.2f m/s, max %.2f m/s\n",
                item,
                mean(object, signal = item),
                median(object, signal = item),
                min(object, signal = item),
                max(object, signal = item)),
        sep = "")
  }
  for (item in names(object$wind_dir)) {
    cat(sprintf("Wind direction %s: mean %.2f, median %.2f\n",
                item,
                mean(object, signal = item),
                median(object, signal = item)),
        sep = "")
  }
  for (item in names(object$temp)) {
    cat(sprintf("Temperature %s: mean %.2f C, median %.2f C, min %.2f C, max %.2f C\n",
                item,
                mean(object, signal = item),
                median(object, signal = item),
                min(object, signal = item),
                max(object, signal = item)),
        sep = "")
  }
  for (item in names(object$pressure)) {
    cat(sprintf("Pressure %s: mean %.2f hPa, median %.2f hPa, min %.2f hPa, max %.2f hPa\n",
                item,
                mean(object, signal = item),
                median(object, signal = item),
                min(object, signal = item),
                max(object, signal = item)),
        sep = "")
  }
}


