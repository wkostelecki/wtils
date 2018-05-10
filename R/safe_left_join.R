#' safe_left_join
#' @description left join that fails if a row in x is either duplicated or
#'   unmatched.
#' @param x table to join
#' @param y table to join
#' @param by a character vector of column names to join by.
#' @export

safe_left_join = function (x, y, by = NULL) {
  rows_start = nrow(x)

  if (is.null(by)) {
    by = intersect(names(x), names(y))
  } else {
    by = as.character(by)
  }

  y[["..1.."]] = 1
  x = left_join(x, y, by)

  if (nrow(x) > rows_start) {
    stop("Rows have been duplicated in 'safe' left join")
  }

  if (any(ind <- is.na(x[["..1.."]]))) {
    sample = sample(which(ind), min(10, sum(ind)))
    examples = distinct(x[sample, by, drop = FALSE])
    print(examples)
    stop(sprintf("Failed to match %d rows in x.", sum(ind)))
  }

  x[["..1.."]] = NULL

  x

}
