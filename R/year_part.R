#' year_part
#' @description convert a date vector to labelled year parts
#' @param x A date vector.
#' @examples
#' year_part(seq(as.Date("2001-01-01"), by = "month", length.out = 16))
#' @export
year_part = function(x) {
  max_month = lubridate::month(max(x, na.rm = TRUE))

  if (max_month == 1) {
    jan_to = "Jan"
    to_dec = "Feb-Dec"
    y = ifelse(lubridate::month(x) == 1, jan_to, to_dec)
  } else if (max_month == 12) {
    jan_to = "Jan-Dec"
    to_dec = NULL
    y = rep(jan_to, length(x))
  } else {
    jan_to = paste0("Jan-", month.abb[max_month])
    to_dec = paste0(month.abb[max_month + 1], "-Dec")
    y = ifelse(lubridate::month(x) <= max_month,
           jan_to,
           to_dec)
  }
  factor(y, c(jan_to, to_dec))
}
