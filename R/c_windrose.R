#' c_windrose
#'
#' Plot a windrose from c_mseries object or a numeric vector.
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param signals character, a name of a wind direction signal(s). If NULL, the main\cr
#' signal is used.
#' @param col color
#'
#' @importFrom magrittr %>%
#'
#' @export
c_windrose <- function(cx, signals = NULL, col = "blue") {
  # check if cx is c_mseries object
  if (!is.c_mseries(cx)) {
    stop("cefiro package error: Invalid input format! Argument is not a c_mseries object.",
         call. = FALSE)
  }
  # check if more than two signals are provided
  if (length(signals) > 2) {
    stop("cefiro package error: Invalid input! Too many wind direction signals.",
         call. = FALSE)
  }
  signals_names <- c(names(cx$wind_dir))
  if (sum(signals %in% signals_names) != length(signals)) {
    stop("cefiro package error: Invalid input! One of signals does not exist in c_mseries object.",
         call. = FALSE)
  }

  # create breaks
  breaks <- seq(0,360,30)

  if (is.null(signals)) {
    dfplot <- xts::as.xts(cx$mdata[, cx$main_wind_dir])
    signals <- as.character(cx$main_wind_dir)
    signal_name <- paste0("Wind direction ", signals, " at ", as.numeric(cx$wind_dir[signals]), " m")
    # create bins according to main wind direction series
    dfplot_binned <- cut(dfplot[, signals], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
  } else if (length(signals) == 1) {
    dfplot <- xts::as.xts(cx$mdata[, signals])
    dfplot_binned <- cut(dfplot[, signals[1]], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
    signals <- as.character(signals)
    signal_name <- paste0("Wind direction ", signals, " at ", as.numeric(cx$wind_dir[signals]), " m")
    h <- as.numeric(cx$wind_dir[signals])
    if (sum(!is.na(names(cx$wind_dir[signals]))) == length(names(cx$wind_dir[signals]))) {
      h <- as.numeric(cx$wind_dir[signals])
    }
  } else if (length(signals) == 2) {
    dfplot <- xts::as.xts(cx$mdata[, signals])
    h_names <- paste0(signals, " at ", as.character(cx$wind_dir[signals]), " m")
    signal_name <- paste0("Wind directions ", stringr::str_c(h_names, collapse = ", "))
  }

  if (length(signals) == 1) {
    levels(dfplot_binned) <- as.character(breaks)
    xdatap <- data.frame(xdir = as.numeric(dfplot[, signals]), xbinned = dfplot_binned) %>%
        dplyr::filter(xdir != 360) %>%
        dplyr::group_by(xbinned) %>%
        dplyr::summarise(dir_count = dplyr::n()) %>%
        dplyr::mutate(dir_per = 100 * dir_count / sum(dir_count)) %>%
        dplyr::mutate(wind_vane = signals)
  } else if (length(signals) == 2) {
    # first wind vane signal
    dfplot_binned <- cut(dfplot[, signals[1]], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
    levels(dfplot_binned) <- as.character(breaks)
    xdatap1 <- data.frame(xdir = as.numeric(dfplot[, signals[1]]), xbinned = dfplot_binned) %>%
      dplyr::filter(xdir != 360) %>%
      dplyr::group_by(xbinned) %>%
      dplyr::summarise(dir_count = dplyr::n()) %>%
      dplyr::mutate(dir_per = 100 * dir_count / sum(dir_count)) %>%
      dplyr::mutate(wind_vane = signals[1])

    # second wind vane signal
    dfplot_binned <- cut(dfplot[, signals[2]], breaks = c(breaks, 390), ordered_result = TRUE, right = FALSE)
    levels(dfplot_binned) <- as.character(breaks)
    xdatap2 <- data.frame(xdir = as.numeric(dfplot[, signals[2]]), xbinned = dfplot_binned) %>%
      dplyr::filter(xdir != 360) %>%
      dplyr::group_by(xbinned) %>%
      dplyr::summarise(dir_count = dplyr::n()) %>%
      dplyr::mutate(dir_per = 100 * dir_count / sum(dir_count)) %>%
      dplyr::mutate(wind_vane = signals[2])
    xdatap <- rbind(xdatap1, xdatap2)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_bar(data = xdatap,
                      ggplot2::aes(x = xbinned, y = dir_per, fill = wind_vane),
                      stat = "identity",
                      position = "dodge2") +
                      ggplot2::scale_fill_brewer(palette="Paired") +
    ggplot2::coord_polar(start = -(30/2)*(pi/180)) +
    ggplot2::labs(fill = "",
                  x = signal_name,
                  y = "%") +
    ggplot2::scale_y_continuous(
      breaks = c(0, 5, 10, 15),
      limits = c(0, 20)
    ) +
    ggplot2::theme_minimal()


  return(p)

}
