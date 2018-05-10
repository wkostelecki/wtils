#' side_summary
#' @export
#' @param data A data.frame.
#' @param extra A character vector of extra columns to add to summary. Default
#'   is NULL. Options are unique and sample.
#' @importFrom stats median reorder
#' @examples
#' side_summary(mtcars)
#' side_summary(mtcars, extra = "unique")
#' side_summary(mtcars, extra = c("unique", "sample"))
#'
#' library(dplyr)
#' mtcars %>% mutate(n = rnorm(n()), x = runif(n()) > 0.5) %>% side_summary
#' mtcars %>% mutate(vs = as.character(vs)) %>% side_summary
side_summary = function(data, extra = NULL) {

  if (any(extra == "all")){
    extra = c("unique", "sample")
  }

  summaries = sapply(data, function(x) column_summary(x),
                     simplify = FALSE)

  out = cbind(Column = names(summaries),
              Class = unname(sapply(data, function(x) paste(class(x),
                                                            collapse = ", "))),
              dplyr::bind_rows(unname(summaries)))

  if (any("unique" == extra) | any("sample" == extra)) {
    unique_vals = lapply(data,
                         function(x) {
                           dplyr::count(data.frame(x = x,
                                                   stringsAsFactors = FALSE),
                                        x)
                         })
  }

  if (any("unique" == extra)) {
    out = cbind(out,
                Unique = unname(sapply(unique_vals, function(x) nrow(x))))
  }

  if (any("sample" == extra)) {
    out = cbind(out,
                Sample = unname(sapply(
                  unique_vals,
                  function(x) {
                    paste(sample(paste0(x[["x"]], " (", x[["n"]], ")"), min(5, nrow(x))), collapse = ", ")
                  }
                ))
    )
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
             Non0 = NA_real_,
             gt0 = NA_real_,
             NAs = sum(is.na(x)),
             stringsAsFactors = FALSE)

}

column_summary.numeric = function(x, ...){
  data.frame(Min = min(x, na.rm = TRUE),
             Med = median(x, na.rm = TRUE),
             Mean = mean(x, na.rm = TRUE),
             Max = max(x, na.rm = TRUE),
             Sum = sum(as.numeric(x), na.rm = TRUE),
             Non0 = sum(x != 0, na.rm = TRUE),
             gt0 = sum(x > 0, na.rm = TRUE),
             NAs = sum(is.na(x)),
             stringsAsFactors = FALSE)
}

column_summary.logical = function(x, ...){
  data.frame(Min = min(x, na.rm = TRUE),
             Med = median(as.numeric(x), na.rm = TRUE),
             Mean = mean(x, na.rm = TRUE),
             Max = max(x, na.rm = TRUE),
             Sum = sum(x, na.rm = TRUE),
             Non0 = sum(x, na.rm = TRUE),
             gt0 = sum(x, na.rm = TRUE),
             NAs = sum(is.na(x)),
             stringsAsFactors = FALSE)
}
