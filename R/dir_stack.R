#' dir_stack
#'
#' @param path Path to a directory.
#' @param pattern Regular expression to match on file names.
#' @param file_paths A vector of files paths. If supplied, /code{path},
#'   /code{pattern} and \code{recursive} are ignored.
#' @param ind Logical or numeric indices used to subset file_paths.
#' @param recursive passed to \code{list.files()}
#' @param func A function for processing object read in by \code{read_func()}
#' @param read_func A function for reading in files specified in \code{file_paths}.
#' @param verbose \code{TRUE} or \code{FALSE}. Default is \code{FALSE}.
#'
#' @return A data.frame.
#' @importFrom utils read.csv
#' @export
dir_stack = function (path = ".",
                      pattern = "\\.csv$",
                      file_paths = NULL,
                      ind = NULL,
                      recursive = FALSE,
                      func = NULL,
                      read_func = function(x) read.csv(x, stringsAsFactors = FALSE),
                      verbose = FALSE) {

  if (verbose) {
    start_time = Sys.time()
  }

  if (is.null(file_paths)) {
    file_paths = list.files(path,
                            pattern,
                            full.names = TRUE,
                            recursive = recursive)
  }

  if (length(file_paths) == 0) {
    stop(sprintf("No files matching pattern \"%s\"",
                 pattern))
  }

  if (!is.null(ind)) {
    file_paths = file_paths[ind]
  }

  data = vector("list", length(file_paths))
  for (i_file in 1:length(file_paths)) {
    if (verbose) {
      writeLines(file_paths[i_file])
      writeLines(sprintf("%d of %d", i_file, length(file_paths)))
    }

    data[[i_file]] = read_func(file_paths[i_file])

    if (verbose) {
      writeLines(sprintf("Table Dimensions: %d x %d", nrow(data[[i_file]]),
                         ncol(data[[i_file]])))
    }
    if (!is.null(func)) {
      x = data[[i_file]]
      data[[i_file]] = func(x)
      if (verbose) {
        writeLines(sprintf("Dimensions After Processing: %d x %d",
                           nrow(data[[i_file]]), ncol(data[[i_file]])))
      }
    }
    if (nrow(data[[i_file]]) > 0) {
      data[[i_file]][["source_file"]] = gsub("^.*/|^.*\\\\", "",
                                             file_paths[i_file])
    }
  }

  data = dplyr::bind_rows(data)

  if (verbose) {
    print(Sys.time() - start_time)
  }

  data

}




unused_name = function(x,
                       y = c(),
                       y_unique = FALSE,
                       warning = FALSE){

  y = if (y_unique) unique(y) else y

  out = make.names(c(y, x), unique = TRUE)[(1:length(x)) + length(y)]

  ind = x != out
  if (warning && any(ind)){
    warning(sprintf("Changed names:\n %s\n to\n %s",
                    paste(x[ind], collapse = ", "),
                    paste(out[ind], collapse = ", ")))
  }

  out

}
