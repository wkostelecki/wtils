#' count_elements
#' @param x A character vector. Usually, the output from \code{readLines()}.
#' @param sep A separator used in \code{x}.
#' @examples
#' text = c("a,b", "b,d", 'a,",,d"', "a, b, c, d")
#' count_elements(text)
#' @export
count_elements = function(x, sep = ",", quote = NULL) {
  if (!is.null(quote)) {
    x = gsub(paste0(quote, ".*?", quote), "", x)
  }
  x = paste0(x, sep, "x")
  sapply(strsplit(x, sep), length) - 1
}
