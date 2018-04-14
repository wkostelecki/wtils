
#' na_profile
#' @export
#' @importFrom magrittr %>%
#' @examples
#' na_profile(airquality)
#' na_profile(airquality, all_cols = TRUE)
#' na_profile(mtcars)
#' na_profile(mtcars, all_cols = TRUE)
na_profile = function(data, all_cols = FALSE) {

  stopifnot(!(any(c("n()", "na") %in% names(data))))

  x = lapply(data, function(x) ifelse(is.na(x), NA_character_, ".")) %>%
    do.call(cbind, .) %>%
    as.data.frame

  top = sapply(x, function(x) sum(is.na(x)))

  ord = order(top, decreasing = TRUE)

  if (!all_cols) {
    ord = ord[seq_len(sum(top > 0))]
  }

  x = x[ord] %>%
    dplyr::mutate(..na_profile.. = 1) %>%
    dplyr::group_by_at(vars(names(.))) %>%
    dplyr::summarize(n()) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::desc(`n()`)) %>%
    dplyr::select(-..na_profile..) %>%
    as.data.frame

  x[["na"]] = apply(x[, seq_len(ncol(x) - 1)], 1, function(x) sum(is.na(x)))

  rbind(c(paste(top[ord], rep("NAs", length(ord))), "", ""),
        x %>%
          dplyr::mutate_all(as.character))

}



