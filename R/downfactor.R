#' downfactor
#' @param x A character or factor vector
#' @param y A numeric vector
#' @param n Number of factors to retain
#' @export
downfactor = function(x, y = rep(1, length(x)), n = 5) {
  fct = reorder(factor(x), -y, function(x) sum(x, na.rm = TRUE))
  n = pmin(n, length(levels(fct)))
  forcats::fct_other(fct, levels(fct)[seq_len(n)])
}
