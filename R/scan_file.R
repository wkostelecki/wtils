#' scan_file
#' @param file path to a text file
#' @param n chunk size
#' @param read_func function to use for reading file
#' @param header logical indicating whether file has a header
#' @param agg_func aggregation function for each chunk
#' @param verbose logical
#' @importFrom utils read.csv
#' @export
scan_file = function(file,
                     n = 10000,
                     read_func = function(file, nrows, header) {
                       read.csv(file, nrows = nrows, header = header)
                     },
                     header = FALSE,
                     agg_func = econ::side_summary,
                     verbose = FALSE){

  con = file(file, open = "r")
  on.exit(close(con))

  count = 0
  i = 0
  out = vector("list", 1)

  if (verbose) start = Sys.time()

  if (header) {
    header_names = names(read_func(con, 1, header = TRUE))
    header = FALSE
  }

  while (TRUE){

    tmp = read_func(con, n, header)

    if (exists("header_names", environment())) {
      names(tmp) = header_names
    }

    len = if (is.data.frame(tmp)) nrow(tmp) else length(tmp)

    count = count + len
    i = i + 1

    if (verbose) cat(count, "\n")

    out[[i]] = agg_func(tmp)

    if (len < n){
      break
    }

  }

  if (verbose) print(Sys.time() - start)

  out

}
