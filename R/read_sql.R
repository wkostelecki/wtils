#' read_sql
#' @param file relative or absolute path to SQL query file.
#'
#' @export
read_sql = function(file) {

  paste(readLines(file), collapse = "\n")

}
