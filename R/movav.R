#' Moving average of a vector
#' @description Calculates the moving average of a vector.
#' @param x A numeric vector.
#' @param n Number of values to average.
#' @param na.rm A logical. Whether or not to treat NAs as 0.
#' @param na.start Whether starting elements without enough values to average
#'   should calculate to NA. Default is \code{FALSE}.
#' @examples
#' movav(runif(100))
#' @export
movav = function(x,
                 n = 52,
                 na.rm = TRUE,
                 na.start = FALSE) {
  if (na.rm) {
    x[is.na(x)] = 0
  }
  len = length(x)
  if (len >= n) {
    y = as.numeric(stats::filter(x, rep(1 / n, n), sides = 1))
  } else {
    y = rep(NA_real_, len)
  }
  ind = seq_len(pmin(len, n))
  if (!na.start) {
    y[ind] = cumsum(x[ind]) / ind
  }
  y
}
