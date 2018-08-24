#' get_headers
#' @description extracts header names from unparsed data.frame (usually from Excel)
#' @param data A data.frame.
#' @param header_row A scalar numeric (row containing headers).
#' @param cols A numeric vector (column indices).
get_headers = function(data, header_row, cols,
                       fill = TRUE,
                       make.names = TRUE) {
  y = data[header_row, cols] %>%
    as.character %>%
    data_frame(x = .)
  if (fill) y = tidyr::fill(y, x, .direction = "down")
  y = y %>%
    pull(x)
  if (make.names) y = make.names(y, unique = TRUE)
  y
}
