
#' count_lines
#' @description counts the number of lines in a text file
#' @param file path to text file
#' @param n chunk size to read in at a time
#' @param verbose logical
#' @export
count_lines = function(file, n = 10000, verbose = FALSE){
  con = file(file, open = "r")
  on.exit(close(con))
  count = 0
  if (verbose) start = Sys.time()
  while (TRUE){
    tmp = readLines(con, n)
    count = count + length(tmp)
    if (verbose) cat(count, "\n")
    if (length(tmp) < n){
      break
    }
  }
  if (verbose) print(Sys.time() - start)

  count
}
