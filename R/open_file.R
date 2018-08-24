#' @export
open_file_addin = function () {
  if (Sys.getenv("RSTUDIO") != "1") {
    stop("Only works in RStudio")
  }
  context = rstudioapi::getActiveDocumentContext()
  file = gsub("^ *| *$", "", context[["selection"]][[1]][["text"]])
  file = gsub("'|\"", "", file)
  file = gsub("/*$|\\\\*$", "", file)
  if (!file.exists(file)) {
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
