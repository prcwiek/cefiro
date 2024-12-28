#' c_find_weibull
#'
#' Find the scale c factor and the shape k factor of the Weibull distribution
#' for c_mseries, a selected wind speed signal and a given range of k.
#'
#' @param cx c_mseries object with data and information about wind measurement\cr
#' or a numeric vector.
#' @param signal character, a name of a wind speed signal. If NULL, the main\cr
#' signal is used.
#' @param kmin numeric, minimum value of the shape k factor.
#' @param kmax numeric, maximum value of the shape k factor.
#' @param eps numeric, accuracy, the default value is 0.000001.
#' @param iter numeric, number of iterations.
#'
#' @return list with the scale c factor and the shape k factor\cr
#' of the Weibull distribution.
#'
#' @examples
#' \dontrun{
#' c_find_weibull(cx)
#' c_find_weibull(cx, signal = "WS77")
#' c_find_weibull(cx, signal = "WS77", kmin = 1, kmax = 8, eps = 0.000001, iter = 50)
#' }
#'
#' @export
c_find_weibull <- function(cx = NULL, signal = NULL,
                           kmin = 1, kmax = 8,
                           eps = 0.000001,
                           iter = 50) {

  # check if cx is c_mseries object
  if (is.null(cx) & !is.c_mseries(cx)) {
    stop("cefiro package error: Invalid input format! Argument is not a c_mseries object.",
         call. = FALSE)
  }

  # check if more than one signal is provided
  if (!is.null(signal) & length(signal) > 1) {
    stop("cefiro package error: Invalid input! Only one signal possible.",
         call. = FALSE)
  }

  # get data series
  if (!is.null(cx) & is.c_mseries(cx)) {
    if (!is.null(signal)) {
      if (signal %in% names(cx$wind_speed)) {
        x <- as.numeric(cx$mdata[,signal])
      } else {
        stop("cefiro package error: Invalid input format! Signal is not a wind speed.", call. = FALSE)
      }
    } else {
      x <- as.numeric((cx$mdata[,cx$main_wind_speed]))
    }
  } else if (is.numeric(cx) & is.vector(cx)) {
    x <- cx
  } else {
    stop("cefiro package error: Invalid input format! Argument is not c_mseries object or a vector.", call. = FALSE)
  }

  # remove NA from the input data
  x <- x[!is.na(x)]

  # estimate shape factor k
  k <- bisection(x, kmin, kmax, eps, iter)

  if (!is.null(k)) {
    n <- length(x)
    ws_mean <- mean(x)

    c <- ws_mean * (0.586 + 0.433/k)^(-1/k)

    return(list(c = c, k = k))
  } else return(NULL)
}

k_estimator <- function(x, k) {
  ke <- mean(x^3) / mean(x)^3
  ke * (gamma(1 + 1/k)^3) - gamma(1 + 3/k)
}

bisection <- function(x, kmin, kmax, eps, iter) {
  # First estimation
  fkmin <- k_estimator(x, kmin)
  fkmax <- k_estimator(x, kmax)
  if (fkmin * fkmax > 0) {
    stop("cefiro package error: Both estimated k values are greater than zero!", call. = FALSE)
  }
  for (j in 1:iter) {
    k <- (kmin+kmax) / 2
    fk <- k_estimator(x, k)
    fkk <- (fkmax - fkmin) / (kmax - kmin)
    #message(sprintf("Value of k_estimator in point %.4f is %.4f\n", k, fk))
    if (abs(fk/fkk) - eps > 0) {
      if (fk *  fkmin < 0) {
        kmax <- k
        fkmax <- fk
      } else {
        if (fk * fkmin == 0) {
          return(k)
        }
        kmin <- k
        fkmin <- fk
      }
    } else {
      return(k)
    }
  }
  retrun(NULL)
}




