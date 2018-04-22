

#' @export

safe_left_join = function (x, y, by = NULL) {
  rows_start = nrow(x)
  if (is.null(by)) {
    by = intersect(names(x), names(y))
  } else {
    by = as.character(by)
  }
  x = left_join(x, mutate(y, ..1.. = 1), by)
  if (nrow(x) > rows_start) {
    stop("Rows have been duplicated in 'safe' left join")
  }
  if (any(ind <- is.na(x[["..1.."]]))) {
    sample = sample(which(ind), min(10, sum(ind)))
    examples = distinct(x[sample, by, drop = FALSE])
    print(examples)
    stop(sprintf("Failed to match %d rows in x.", sum(ind)))
  }
  x %>% select(-..1..)
}
