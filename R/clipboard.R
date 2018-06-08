
#' clipboard
#'
#' @param x A data.frame.
#' @param alloc Memory to allocate for copied data.frame. Should be a power of
#'   2.
#'
#' @return A data.frame
#' @export
#' @examples
#' clipboard(mtcars)
#' print(clipboard())
clipboard = function (x = NULL, alloc = 2048, ...) {
  if (is.null(x)) {
    utils::read.table("clipboard", header = TRUE, sep = "\t", ...)
  } else {
    stopifnot(is.data.frame(x))
    stopifnot(is.numeric(alloc))
    stopifnot(log2(alloc) == round(log2(alloc)))
    utils::write.table(x,
                       sprintf("clipboard-%d", alloc),
                       sep = "\t",
                       row.names = FALSE,
                       qmethod = "double")
  }
}
