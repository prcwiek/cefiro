#' c_windrose
#'
#' Plot a windrose from c_mseries object or a numeric vector.
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param signal character, a name of a wind direction signal(s). If NULL, the main\cr
#' signal is used.
#' @param col color
#'
#' @importFrom magrittr %>%
#'
#' @export
c_windrose <- function(cx, signal = NULL, col = "blue") {
  if (!is.c_mseries(cx)) {
    stop("cefiro package error: Invalid input format! Argument is not a c_mseries object.",
         call. = FALSE)
  }
  if (length(signal) > 2) {
    stop("cefiro package error: Invalid input! Too many wind direction signals.",
         call. = FALSE)
  }
  signals_names <- c(names(cx$wind_dir))
  if (sum(signal %in% signals_names) != length(signal)) {
    stop("cefiro package error: Invalid input! One of signals does not exist in c_mseries object.",
         call. = FALSE)
  }

  # create breaks
  breaks <- seq(0,360,30)

  if (is.null(signal)) {
    dfplot <- xts::as.xts(cx$mdata[, cx$main_wind_dir])
    signal <- as.character(cx$main_wind_dir)
    signal_name <- paste0("Wind direction ", signal, " at ", as.numeric(cx$wind_dir[signal]), " m")
    # create bins according to main wind direction series
    dfplot_binned <- cut(dfplot[, signal], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
  } else if (length(signal) == 1) {
    dfplot <- xts::as.xts(cx$mdata[, signal])
    dfplot_binned <- cut(dfplot[, signal[1]], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
    signal <- as.character(signal)
    signal_name <- paste0("Wind direction ", signal, " at ", as.numeric(cx$wind_dir[signal]), " m")
    h <- as.numeric(cx$wind_dir[signal])
    if (sum(!is.na(names(cx$wind_dir[signal]))) == length(names(cx$wind_dir[signal]))) {
      h <- as.numeric(cx$wind_dir[signal])
    }
  } else if (length(signal) == 2) {
    dfplot <- xts::as.xts(cx$mdata[, signal])
    # create bins according to first wind direction series
    dfplot_binned <- cut(dfplot[, signal[1]], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
    #hs <- as.numeric(cx$wind_dir[signal])
    h_names <- paste0(signal, " at ", as.character(cx$wind_dir[signal]), " m")
    signal_name <- paste0("Wind directions ", stringr::str_c(h_names, collapse = ", "))
  }

  if (length(signal) == 1) {
    levels(dfplot_binned) <- as.character(breaks)
    xdatap <- data.frame(xdir = as.numeric(dfplot[, signal]), xbinned = dfplot_binned) %>%
        dplyr::filter(xdir != 360) %>%
        dplyr::group_by(xbinned) %>%
        dplyr::summarise(dir_count = dplyr::n()) %>%
        dplyr::mutate(dir_per = 100 * dir_count / sum(dir_count)) %>%
        dplyr::mutate(wind_vane = signal)
  } else if (length(signal) == 2) {
    dfplot_binned <- cut(dfplot[, signal[1]], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
    levels(dfplot_binned) <- as.character(breaks)
    xdatap <- data.frame(xdir = as.numeric(dfplot[, signal[1]]), xbinned = dfplot_binned) %>%
      dplyr::mutate(wind_vane = signal[1])
    dfplot_binned <- cut(dfplot[, signal[2]], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
    levels(dfplot_binned) <- as.character(breaks)
    xdatap2 <- data.frame(xdir = as.numeric(dfplot[, signal[2]]), xbinned = dfplot_binned) %>%
      dplyr::mutate(wind_vane = signal[2])
    xdatap <- rbind(xdatap, xdatap2) %>%
      dplyr::filter(xdir != 360) %>%
      dplyr::group_by(xbinned, wind_vane) %>%
      dplyr::summarise(dir_count = dplyr::n()) %>%
      dplyr::mutate(dir_per = 100 * dir_count / sum(dir_count))

  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = xdatap,
                      ggplot2::aes(x = xbinned, y = dir_per, fill = wind_vane),
                      stat = "identity",
                      position = "dodge2") +
                      ggplot2::scale_fill_brewer(palette="Paired") +
    ggplot2::coord_polar(start = -(30/2)*(pi/180)) +
    ggplot2::xlab(signal_name) +
    ggplot2::ylab("%") +
    ggplot2::theme_minimal() +
    ggplot2::labs(fill = "")

  return(p)

}
