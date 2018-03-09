#' side_summary
#' @export
#' @param data A data.frame.
#' @param ... Extra inputs.
#'
#' @examples
#' side_summary(mtcars)
#' side_summary(mtcars, unique = T)
#' side_summary(mtcars, unique = T, sample = T)
side_summary = function(data, ..., unique = FALSE, sample = FALSE){

  summaries = sapply(data, function(x) column_summary(x, ...),
                     simplify = FALSE)

  out = cbind(Column = names(summaries),
              Class = unname(sapply(data, function(x) paste(class(x), collapse = ", "))),
              dplyr::bind_rows(unname(summaries)))

  if (unique | sample) {
    unique_vals = lapply(data, unique)
  }

  if (unique) {
    out = cbind(out,
                Unique = unname(sapply(unique_vals, function(x) length(x))))
  }

  if (sample) {
    out = cbind(out,
                Sample = unname(sapply(unique_vals, function(x) {
                  paste(sample(x, min(5, length(x))), collapse = ", ")
                })))
  }

  out

}

column_summary = function(x, ...){
  UseMethod("column_summary")
}

column_summary.default = function(x, ...){

  data.frame(Min = NA_real_,
             Med = NA_real_,
             Mean = NA_real_,
             Max = NA_real_,
             Sum = NA_real_,
             NAs = sum(is.na(x)),
             stringsAsFactors = FALSE)

}

column_summary.numeric = function(x, ...){
  data.frame(Min = signif(min(x, na.rm = TRUE), 3),
             Med = signif(median(x, na.rm = TRUE), 3),
             Mean = signif(mean(x, na.rm = TRUE), 3),
             Max = signif(max(x, na.rm = TRUE), 3),
             Sum = sum(as.numeric(x), na.rm = TRUE),
             NAs = signif(sum(is.na(x)), 3),
             stringsAsFactors = FALSE)
}
