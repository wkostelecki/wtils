#' read_sql
#' @param file Relative or absolute path to SQL query file.
#'
#' @export
read_sql = function(file) {

  lines = readLines(file)
  lines = sub(" *--.*$", "", lines)
  paste(lines[lines != ""], collapse = "\n")

}
