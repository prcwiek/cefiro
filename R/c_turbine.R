#' c_turbine
#'
#' Create c_turbine object with basic information about a wind turbine.
#' \itemize{
#' \item name, a wind turbine name
#' \item producer
#' \item model, a wind turbine model
#' \item power, a nominal power of wind turbine in kW
#' \item diameter, a rotor diameter of wind turbine
#' \item hh, a hub height of wind turbine in meters
#' \item cut_in, a cut in wind speed in m/s
#' \item cut_out, a cut out wind speed in m/s
#' \item class_IEC, a wind turbine IEC class
#' \item subclass_IEC, a wind turbine subclass IEC
#' \item ws, wind speed bins, values in m/s
#' \item p, power in kW
#' \item cp, power coefficient
#' \item ct, thrust coefficient
#' \item ro, air density in kg/m3
#' }
#'
#' @param name character; a name of wind turbine.
#' @param producer character; a producer name.
#' @param model character; a wind turbine model.
#' @param power character; a nominal power of wind turbine.
#' @param diameter numeric; a rotor diameter of wind turbine.
#' @param hh numeric; a hub height of wind turbine.
#' @param cut_in numeric; a cut-in wind speed of wind turbine
#' @param cut_out numeric; a cut-out wind speed of wind turbine
#' @param class_IEC character; a wind turbine IEC class
#' @param subclass_IEC character; a wind turbine IEC subclass
#' @param ws numeric; a vector with wind speed bins
#' @param p numeric; a vector with power for each wind speed bin
#' @param cp numeric; a vector with power coefficient for each wind speed bin
#' @param ct numeric; a vector with thrust coefficient for each wind speed bin
#' @param ro numeric: air density
#'
#' @return c_turbine object with information about a wind turbine
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
c_turbine <- function(name, producer, model, power, diameter,
                      hh, cut_in, cut_out,
                      class_IEC, subclass_IEC,
                      ws, p, cp, ct, ro) {
  UseMethod("c_turbine")
}

#' @export
c_turbine.default <- function(name = NULL, producer = NULL, model = NULL, power = NULL, diameter = NULL,
                      hh = NULL, cut_in = NULL, cut_out = NULL,
                      class_IEC = NULL, subclass_IEC = NULL,
                      ws = NULL, p = NULL, cp = NULL, ct = NULL, ro = NULL) {
  # check if minimal set of data is provided
  if (is.null(ws) | !is.numeric(ws) | is.null(p) | !is.numeric(p)) {
    stop("cefiro package error: incorrect wind speeds vector ws or/and power vector p!", call. = FALSE)
  }
  if (is.null(diameter) | !is.numeric(diameter)) {
    stop("cefiro package error: incorrect diameter value!", call. = FALSE)
  }

  # check if ws and p vectors has the same length
  if (length(ws) != length(p)) {
    stop("cefiro package error: ws and p vectors are not equal!", call. = FALSE)
  }

  # check numeric values
  if (is.character(hh)) {
    stop("cefiro package error: incorrect hh hub height value!", call. = FALSE)
  }
  if (is.character(ro)) {
    stop("cefiro package error: incorrect ro air density value!", call. = FALSE)
  }

  # correct NULL parameters
  if (is.null(name)) name <- "unknown"
  if (is.null(producer)) producer <- "unknown"
  if (is.null(model)) model <- "unknown"
  if (is.null(power)) power <- max(p)
  if (is.null(hh)) hh <- 125
  if (is.null(cut_in)) cut_in <- min(ws)
  if (is.null(cut_out)) cut_out <- max(ws)
  if (is.null(class_IEC)) class_IEC <- "unknown"
  if (is.null(subclass_IEC)) subclass_IEC <- "unknown"
  if (is.null(ro)) ro <- 1.225

  # calculate power coefficient Cp values
  if (is.null(cp)) {
    cp <- 1000 * p / (0.5 * ro * ws^3 * pi  * (diameter/2)^2)

  }

  structure(list(name = name, producer = producer, model = model,
                 power = power, diameter = diameter,
                 class_IEC = class_IEC, subclass_IEC = subclass_IEC,
                 hh = hh, cut_in = cut_in, cut_out = cut_out,
                 ws = ws, p = p, cp = cp, ct = ct, ro = ro),
            class = "c_turbine")
}

#' @export
print.c_turbine <- function(x, ...) {
  if (!is_c_turbine(x)) {
    stop("cefiro package error: Invalid input format! Argument is not a c_turbine.", call. = FALSE)
  }

  cat("Wind turbine name: ", x$name, "\n")
  cat("Producer: ", x$producer, "\n")
  cat("Model: ", x$model, "\n")
  cat(sprintf("Nominal power: %.2f MW\n", x$power/1000), sep = "")
  cat(sprintf("Rotor diameter: %.2f m\n", x$diameter), sep = "")
  cat(sprintf("Hub height: %.2f m\n", x$hh), sep = "")
  cat(sprintf("Cut-in wind speed: %.2f m/s\n", x$cut_in), sep = "")
  cat(sprintf("Cut-out wind speed: %.2f m/s\n", x$cut_out), sep = "")
  cat("IEC Class: ", x$class_IEC, "\n")
  cat("IEC Subclass: ", x$subclass_IEC, "\n")
  cat(sprintf("Air density: %.3f m/s\n", x$ro), sep = "")
  cat("------------------------------\n")
  cat("Wind speed\tPower\t\tCp\tCt\n")
  cat(sprintf("%.2f m/s\t%.2f kW\t%.3f\t%.3f\n", x$ws, x$power, x$cp, x$ct), sep = "")

}

#' @export
plot.c_turbine <- function(x,
                           plot_ct = FALSE, pch_ct = 15, col_ct = "orange",
                           plot_cp = FALSE, pch_cp = 14, col_cp = "green",
                           col = "blue", type = "l", lty = "solid", pch = 16,
                           xlab = "Wind speed (m/s)",
                           ylab = "Power (kW)",
                           legend.loc = "topright", ...) {
  if (!is_c_turbine(x)) {
    stop("cefiro package error: Invalid input format! Argument is not a c_turbine.", call. = FALSE)
  }

  par(mar=c(6, 4, 4, 6) + 0.1)

  plot(x$ws, x$p,
       col = col,
       lty = lty,
       pch = 16,
       type = type,
       axes = FALSE,
       xlim = c(0, 30),
       ylim = c(0, x$power),
       xlab = "",
       ylab = "",
       main = paste0(x$producer, " ", x$model))
  axis(2, ylim = c(0, x$power), col = "black", las = 1)
  mtext(ylab, side = 2, line = 3)
  box()

  if (plot_ct & !plot_cp) {
    par(new = TRUE)
    plot(x$ws, x$ct,
         col = col_ct,
         pch = pch_ct,
         type = type,
         axes = FALSE,
         xlim = c(0, 30),
         ylim = c(0, 1),
         xlab = "",
         ylab = "")
    mtext("Thrust coefficient Ct", side = 4, col = col_ct, line=3)
    axis(4, ylim = c(0,1), col = col_ct, col.axis = col_ct, las=1)
  }

  if (plot_cp & !plot_ct) {
    par(new = TRUE)
    plot(x$ws, x$cp,
         col = col_cp,
         pch = pch_cp,
         type = type,
         axes = FALSE,
         xlim = c(0, 30),
         ylim = c(0, 0.6),
         xlab = "",
         ylab = "")
    mtext("Power coefficient Cp", side = 4, col = col_cp, line=3)
    axis(4, ylim = c(0, 0.6), col = col_cp, col.axis = col_cp, las=1)
  }

  ## Draw the wind speed axis
  axis(1, pretty(range(0, 30), 5))
  mtext(xlab, side = 1, col = "black", line = 2.5)

}
