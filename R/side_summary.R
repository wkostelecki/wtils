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

  data.frame(Min = NA_character_,
             Med = NA_character_,
             Mean = NA_character_,
             Max = NA_character_,
             NAs = as.character(signif(sum(is.na(x)), 3)),
             stringsAsFactors = FALSE)

}

column_summary.numeric = function(x, ...){
  data.frame(Min = as.character(signif(min(x, na.rm = TRUE), 3)),
             Med = as.character(signif(median(x, na.rm = TRUE), 3)),
             Mean = as.character(signif(mean(x, na.rm = TRUE), 3)),
             Max = as.character(signif(max(x, na.rm = TRUE), 3)),
             NAs = as.character(signif(sum(is.na(x)), 3)),
             stringsAsFactors = FALSE)
}
