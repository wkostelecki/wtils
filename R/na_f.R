
#' na_f
#' @description run common functions but force all-NA inputs to return NA output
#' @param x vector
#' @param f function
#' @examples
#' lqpm:::na(NA, sum)
#' lqpm:::na(1, sum)
#' lqpm:::na(as.Date(NA), min)
#' lqpm:::na(as.Date("2010-01-01"), min)
na_f = function(x, f) {
  if (all(is.na(x))) return(NA)
  f(x, na.rm = TRUE)
}
