#' downfactor
#' @description Retains/sorts the \code{n} largest levels of a character or
#'   factor vector and converts the remaining levels to \code{"Other"}.
#' @param x A character or factor vector.
#' @param y A numeric vector used for aggregating and ordering factor levels.
#' @param n Number of factor levels to retain.
#' @examples
#' downfactor(letters, n = 10)
#' downfactor(letters, seq_along(letters), n = 10)
#' @export
downfactor = function(x, y = rep(1, length(x)), n = 5) {
  fct = forcats::fct_reorder(factor(x), -y, function(x) sum(x, na.rm = TRUE))
  if (n < length(levels(fct))) {
    fct = forcats::fct_other(fct, levels(fct)[seq_len(n)])
  }
  fct
}
