#' c_weibull
#'
#' Weibull distribution function calculates probability for a given wind speed, a
#' shape factor and a scale factor.
#'
#' @param c scale factor
#' @param k shape factor
#' @param ws wind speed
#' @param ro air density
#' @param ... further arguments passed to or from other methods.
#'
#' @return Function return a list of numeric values of a probability for a given wind speed.
#' Additionally for the given c and k the function returns mean wind speed, median wind speed,
#' maximum probability, wind speed for maximum probability, power density, scale factor,
#' shape factor
#'
#' @examples
#' c_weibull(c = 7.0, k = 2.0, ws = 5.0, ro = 1.225)
#' c_weibull(7, 2, 5, 1.225)
#'
#' @export
c_weibull <- function(c, k, ws = 5.0, ro = 1.225, ...) {
  UseMethod("c_weibull")
}

#' @export
c_weibull.default <- function(c = 7.0, k = 2.0, ws = 5.0, ro = 1.225, ...) {
  wfun <- function(cc = c, kk = k, wwss = ws) {
    round((kk/cc)*((wwss/cc)^(kk-1))*exp(-(wwss/cc)^kk),6)
  }
  # probability for the given parametersa and the wind speed
  probability <- round((k/c)*((ws/c)^(k-1))*exp(-(ws/c)^k),6)
  # mean wind speed
  wsmean <- round(c*gamma(1+1/k),2)
  # median wind speed
  wsmedian <- c*log(2)**(1/k)
  # maximum probability for the distribution with the given parameters
  df_w <- data.frame(V=seq(0,25,0.01),P=seq(0,25,0.01))
  df_w[2:2500,2] <- wfun(cc = c, kk = k, wwss=df_w[2:2500,1])
  mp <- max(df_w[2:2500,2])
  # wind speed at the maximum probability for the distribution with the given parameters
  mwsp <- df_w[which.max(df_w[2:2500,2]),1]
  # power density for the the distribution with the given parameters and the air density ro
  pd <- 0.5*ro*c**3*gamma(1+3/k)
  structure(list(probability = probability, mean_wind_speed = wsmean, median_wind_speed = wsmedian,
                 maximum_probability = mp, c_speed_for_maximum_probability = mwsp,
                 power_density = pd, scale_factor_c = c, shape_factor_k = k), class = "c_weibull")
}

#' @export
print.c_weibull <- function(x,...) {
  cat("Weibull distribution for simple wind energy calculations\n")
  cat("--------------------------------------------------------\n")
  cat(paste("Scale factor c:", format(x$scale_factor_c, digits = 3, zero_print = TRUE), "\n"))
  cat(paste("Shape factor k:", format(x$shape_factor_k, digits = 3), "\n"))
  cat("Mean wind speed:", format(x$mean_wind_speed,2, digits = 3), "m/s\n")
  cat("Median wind speed:", format(x$median_wind_speed,2, digits = 3), "m/s\n")
  cat("Maximum probability:", format(x$maximum_probability, digits = 5), "\n")
  cat("Power density:", format(x$power_density,4, digits = 5), "W/m2\n")
  cat("Wind speed at the highest power density:", format(x$c_speed_for_maximum_probability, digits = 3), " m/s\n")
}

#' @export
mean.c_weibull <- function(x,...){
  x$mean_wind_speed
}

#' @export
median.c_weibull <- function(x,...){
  x$median_wind_speed
}

#' @export
plot.c_weibull <- function(x, col = "blue", type = "l", lwd = 1,
                           xlab = "Wind speed (m/s)",
                           ylab = "Probability",
                           ...){
  fstep <- 0.1
  rmin <- 0
  rmax <- 30
  df_dim = rmax / fstep
  df_wd <- data.frame(ws = seq(0, rmax, fstep), p = seq(0, rmax, fstep))
  df_wd[2:df_dim, 2] <- c_weibull(k = x$shape_factor_k, c = x$scale_factor_c, ws = df_wd[2:df_dim, 1])$probability
  srange <- rmin / fstep
  erange <- rmax / fstep
  df_wdplot <- as.data.frame(df_wd[srange:erange, ])
  plot(df_wdplot$ws, df_wdplot$p,
       col = col,
       type = type,
       lwd = lwd,
       xlab = xlab,
       ylab = ylab,
       ...)
}

#' @export
hist.c_weibull <- function(x, col = "blue", freq = FALSE, ...){
  fstep <- 0.1
  rmin <- 0
  rmax <- 30
  df_dim = rmax / fstep
  df_wd <- data.frame(ws = seq(0, rmax, fstep), p = seq(0, rmax, fstep))
  df_wd[2:df_dim, 2] <- c_weibull(k = x$shape_factor_k, c = x$scale_factor_c, ws = df_wd[2:df_dim, 1])$probability
  srange <- rmin / fstep
  erange <- rmax / fstep
  df_wdplot <- as.data.frame(df_wd[srange:erange, ])
  df_wdplot$p <- round(df_wdplot$p * 1000,0)
  df_wdplot <- df_wdplot[df_wdplot$p != 0,]
  dfhist <- 0
  dfhist <- unlist(append(dfhist, apply(df_wdplot, 1, function(x) matrix(data = x[1], nrow = x[2])[,1])), use.names = FALSE)
    hist(dfhist,
       freq = freq,
       breaks = c(0:30),
       main = "Histogram of Weibull Distribution",
       xlab = "Wind speed m/s",
       ylab = "Probability",
       col = col,
       ...)
}

