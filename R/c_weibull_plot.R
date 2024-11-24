#' c_weibull_plot
#'
#' Plot Weibull distribution from c_mseries object or from a numeric vector\cr
#' or with given c scale factor and k shape factor
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param signal character, a name of a wind speed signal. If NULL, the main\cr
#' signal is used.
#' @param c numeric, scale factor
#' @param k numeric, shape factor
#' @param vmin numeric, minimal wind speed
#' @param vmax numeric, maximum wind speed
#' @param line logical, if TRUE, plot Weibull line
#' @param lcolor character, line color of Weibull distribution
#' @param lsize numeric, line size of Weibull distribution
#' @param hist logical, if TRUE, plot histogram
#' @param hcolor character, histogram color
#'
#' @importFrom magrittr %>%
#'
#' @examples
#' c_weibull_plot(cs)
#' c_weibull_plot(cs, hist = TRUE)
#' c_weibull_plot(c = 7.0, k = 2.0, vmin = 0, vmax = 25, lcolor = "blue")
#' c_weibull_plot(c = 7.0, k = 2.0, vmin = 0, vmax = 25, lcolor = "red", lsize = 1, hist = TRUE)
#' c_weibull_plot(c = 7.0, k = 2.0, vmin = 0, vmax = 30, lcolor = "red", hist = TRUE, hcolor = "blue")
#'
#' @export
c_weibull_plot <- function(cx = NULL, signal = NULL,
                           c = 7.0, k = 2.0,
                           vmin = 0, vmax = 25,
                           line = TRUE,
                           lcolor = "blue",
                           lsize = 1,
                           hist = FALSE,
                           hcolor = "#DD8888") {

  # flag for checking if a plot is generated
  plot_generated <- FALSE

  # if c_mseries is not provided in inputs, create a Weibull, distribution with c & k
  if (is.null(cx)) {
    df_dim = vmax / 0.01
    df_wd <- data.frame(ws = seq(0, vmax, 0.01), p = seq(0, vmax, 0.01))
    df_wd[2:df_dim, 2] <- c_weibull(k = k, c = c, ws = df_wd[2:df_dim, 1])$probability

    s_range <- vmin / 0.01
    e_range <- vmax / 0.01
    df_wdplot <- as.data.frame(df_wd[s_range:e_range, ])

    # main ggplot for all options
    p <- ggplot2::ggplot()

    ### start --> histogram without data
    if (hist) {
      bre <- seq(0, 30, 1)
      labx <- bre[1:length(bre) - 1] + 0.5
      df_wdh <- data.frame(ws = labx,
                           p = c_weibull(k = k, c = c, ws = labx)$probability)

      df_hplot <- transform(df_wdplot, bin = cut(ws, breaks = bre, labels = labx))
      df_hplot$bin <- as.character(df_hplot$bin)
      df_hplot <- df_hplot %>%
        dplyr::group_by(bin) %>%
        dplyr::summarise(p = sum(p) / 100)
      df_hplot$bin <- as.numeric(df_hplot$bin)
      df_hplot <- df_hplot[order(df_hplot$bin), ]

      df_wdplot <- df_wdplot[complete.cases(df_wdplot), ]
      df_hplot <- df_hplot[complete.cases(df_hplot), ]

      p <- p + ggplot2::geom_bar(
          data = df_hplot,
          ggplot2::aes(x = bin, y = p),
          fill = hcolor,
          stat = "identity",
          width = 0.9
        )
    }
    ### end --> histogram without data

    ### start --> line without data
    if (line) {
      p <- p + ggplot2::geom_line(
        data = df_wdplot,
        ggplot2::aes(x = ws, y = p),
        color = lcolor,
        size = lsize,
        stat = "identity"
      )
    }
    ### end --> line without data

    # check if a plot has been created and add formatting or create empty plot
    if (hist | line) {
      p <- p + ggplot2::labs(
        x = "Wind speed (m/s)",
        y = "Probability",
        ) +
        ggplot2::xlim(vmin - 0.5, vmax + 0.5) +
        ggplot2::theme_minimal()
      plot_generated <- TRUE
    }
  } else {
    if (!is.c_mseries(cx)) {
      stop("cefiro package error: Invalid input format! Argument is not a c_mseries object.",
           call. = FALSE)
    }
    # when c_mseries is provided in inputs, get data series
    if (!is.null(cx) & is.c_mseries(cx)) {
      if (!is.null(signal)) {
        if (signal %in% names(cx$wind_speed)) {
          x <- as.numeric(cx$mdata[,signal])
          xtext_s <- paste0(" ", signal, " ")
        } else {
          stop("cefiro package error: Invalid input format! Signal is not a wind speed.", call. = FALSE)
        }
      } else {
        x <- as.numeric((cx$mdata[,cx$main_wind_speed]))
        xtext_s <- paste0(" ", as.character(cx$main_wind_speed), " ")
      }
    } else if (is.numeric(cx) & is.vector(cx)) {
      x <- cx
      xtext_s <- " "
    } else {
      stop("cefiro package error: Invalid input format! Argument is not c_mseries object or a vector.", call. = FALSE)
    }

    # remove NA from the input data
    x <- x[!is.na(x)]

    df_dim = vmax / 0.01
    df_wd <- data.frame(ws = seq(0, vmax, 0.01), p = seq(0, vmax, 0.01))

    # find Weibull parameters
    wp <- cefiro::c_find_weibull(x)

    df_wd[2:df_dim, 2] <- cefiro::c_weibull(k = wp$k, c = wp$c, ws = df_wd[2:df_dim, 1])$probability
    s_range <- vmin / 0.01
    e_range <- vmax / 0.01
    df_wdplot <- as.data.frame(df_wd[s_range:e_range, ])

    bre <- seq(0, 30, 1)
    labx <- bre[1:length(bre) - 1] + 0.5
    x <- as.data.frame(table(cut(x, breaks = bre, labels = labx)))
    x$p <- x$Freq / sum(x$Freq)

    df_hplot <- data.frame(bin = labx, p = x$p)

    df_wdplot <- df_wdplot[complete.cases(df_wdplot), ]
    df_hplot <- df_hplot[complete.cases(df_hplot), ]

    # main ggplot for all options
    p <- ggplot2::ggplot()

    ### start --> histogram with data
    if(hist) {
      p <- p + ggplot2::geom_bar(
        data = df_hplot,
        ggplot2::aes(x = bin, y = p),
        fill = hcolor,
        stat = "identity",
        width = 0.9,
        na.rm = TRUE
      )
    }
    ### end --> histogram with data

    ### start --> line with data
    if (line) {
      p <- p + ggplot2::geom_line(
        data = df_wdplot,
        ggplot2::aes(x = ws, y = p),
        color = lcolor,
        size = lsize,
        stat = "identity"
      )
    }
    ### end --> line with data

    # check if a plot has been created and add formatting or create empty plot
    if (hist | line) {
      p <- p + ggplot2::labs(
        x = paste0("Wind speed", xtext_s, "(m/s)"),
        y = "Probability",
      ) +
        ggplot2::xlim(vmin - 0.5, vmax + 0.5) +
        ggplot2::theme_minimal()
      plot_generated <- TRUE
    }
  }

  # generate an empty plot when line = FALSE and hist = FALSE
  if (!plot_generated) {
      p <- ggplot2::ggplot(
        data = df_wdplot,
        ggplot2::aes(x = ws, y = p)) +
        ggplot2::labs(
          x = "Wind speed (m/s)",
          y = "Probability",
        ) +
        ggplot2::xlim(0, 25) +
        ggplot2::ylim(0, 1) +
        ggplot2::geom_blank() +
        ggplot2::theme_minimal()
    }

  return(p)

}
