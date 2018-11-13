#' open_file_addin
#' @export
#' @examples
#' "inst/extdata/"
#' 'inst/extdata/read_excel2.xlsx'
open_file_addin = function () {

  if (Sys.getenv("RSTUDIO") != "1") {
    stop("Only works in RStudio")
  }

  context = rstudioapi::getSourceEditorContext()
  line = context$contents[context$selection[[1]]$range$start[1]]

  start1 = substring(line, 1, context$selection[[1]]$range$start[[2]] - 1)
  end1 = substring(line,
                   context$selection[[1]]$range$start[[2]],
                   nchar(line))

  start = sub("^.*[\"']", "", start1)
  end = sub("[\"'].*$", "", end1)

  file = paste0(start, end)

  if (!file.exists(file) & !dir.exists(file)) {
    warning("File does not exist.")
    return(invisible(NULL))
  }

  open_file(file)

}

open_file = function(file){
  file = normalizePath(file, winslash = "/", mustWork = TRUE)
  system(sprintf("open \"%s\"", file))
  invisible(NULL)
}
