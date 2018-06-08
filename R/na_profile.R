
#' na_profile
#' @param data A data.frame.
#' @param all_cols A character vector of column names.
#' @export
#' @importFrom magrittr %>%
#' @import dplyr
#' @examples
#' na_profile(airquality)
#' na_profile(airquality, all_cols = TRUE)
#' na_profile(mtcars)
#' na_profile(mtcars, all_cols = TRUE)
na_profile = function(data, all_cols = FALSE) {

  stopifnot(!(any(c("n()", "na") %in% names(data))))

  x = lapply(data, function(x) ifelse(is.na(x), NA_character_, "."))
  x = do.call(cbind, x) %>%
    as.data.frame

  top = sapply(x, function(x) sum(is.na(x)))

  ord = order(top, decreasing = TRUE)

  if (!all_cols) {
    ord = ord[seq_len(sum(top > 0))]
  }

  group_cols = c(names(x)[ord], "..na_profile..")

  `n()` = NULL

  x = x[ord] %>%
    dplyr::mutate(..na_profile.. = 1) %>%
    dplyr::group_by_at(dplyr::vars(group_cols)) %>%
    dplyr::summarize(n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(`n()`)) %>%
    as.data.frame
  x[["..na_profile.."]] = NULL
  x[["na"]] = apply(x[, seq_len(ncol(x) - 1)], 1, function(x) sum(is.na(x)))

  rbind(c(paste(top[ord], rep("NAs", length(ord))), "", ""),
        x %>%
          dplyr::mutate_all(as.character))

}



