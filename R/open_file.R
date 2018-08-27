#' @export
open_file_addin = function () {
  if (Sys.getenv("RSTUDIO") != "1") {
    stop("Only works in RStudio")
  }
  context = rstudioapi::getSourceEditorContext()
  line = context$contents[context$selection[[1]]$range$start[1]]
  start = sub('^.*?"', "",
              substring(line, 1, context$selection[[1]]$range$start[[2]]))
  end = sub('".*?$', "",
            substring(line,
                      context$selection[[1]]$range$start[[2]],
                      nchar(line)))
  if (nchar(start) > 0) {
    start = substring(start, 1, nchar(start) - 1)
  } else if (nchar(end) > 0) {
    end = substring(end, 2, nchar(end))
  }
  file = paste0(start, end)
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



# "R/count_elements.R"
