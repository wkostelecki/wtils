#' xsummary
#' @description intersection summary
#' @param ... Vectors for intersection summary
#' @param sample Number of samples to show from each group.
#' @export

xsummary = function(..., sample = 2) {
  x = list(...)
  if (is.null(names(x)) || any(names(x) == "")) {
    names(x) = paste0("V", seq_along(x))
  }
  purrr::map2(x, names(x),
              function(x, y) data.frame(name = y,
                                        value = as.character(x),
                                        stringsAsFactors = FALSE)) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(name, value) %>%
    dplyr::group_by(value) %>%
    dplyr::summarize(name = paste(unique(name), collapse = ", "),
                     n = n()) %>%
    dplyr::group_by(name) %>%
    dplyr::summarize(Unique = n(),
                     Overlap = sum(n),
                     sample = paste(sample(value, sample), collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(Name = name,
                     Unique = Unique,
                     `% Unique` = paste0(round(100 * Unique / sum(Unique)), "%"),
                     Overlap = Overlap,
                     `% Overlap` = paste0(round(100 * Overlap / sum(Overlap)), "%"),
                     Sample = sample)

}
