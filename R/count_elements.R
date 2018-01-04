#' @export
count_elements = function(x, sep = ",") {
  x = paste0(x, sep, "x")
  sapply(strsplit(x, sep), length) - 1
}