#' count_elements
#' @param text A character vector. Usually, the output from \code{readLines()}.
#' @param sep A separator used in \code{x}.
#' @param quote Quote character.
#' @examples
#' text = c("a,b,c", 'a,",,b",c')
#' count_elements(text)
#' count_elements(text, ",", '')
#' @export
count_elements = function(text, sep = ",", quote = "\"") {
  if (!is.null(quote)) {
    text = gsub(paste0(quote, ".*?", quote), "", text)
  }
  text = paste0(text, sep, "x")
  sapply(strsplit(text, sep), length) - 1
}
