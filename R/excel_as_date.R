#' excel_as_date
#' @description Converts Excel numeric dates to Date class
#' @param x A numeric vector.
#' @export
excel_as_date = function (x) {
  if (is.factor(x)) {
    x = as.character(x)
  }
  x = floor(as.numeric(x))
  x = ifelse(x == 60, NA, x)
  x = ifelse(x > 60, x - 1, x)
  x = as.Date(x, origin = as.Date("1900-01-01") - 1)
  x
}
