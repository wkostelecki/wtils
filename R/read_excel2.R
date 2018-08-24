#' read_excel2
#' @export
#' @examples
#' file = "inst/extdata/read_excel2.xlsx"
#' sheet = "format1"
#' data_row = 4
#' read_excel2(file, sheet, data_row, val_header_row = 2)

#' file = "inst/extdata/read_excel2.xlsx"
#' sheet = "format2"
#' data_row = 5
#' read_excel2(file, sheet, data_row, val_header_row = 3, gather = TRUE)
read_excel2 = function(path, sheet = NULL,
                       data_row = 2,
                       cat_header_row = data_row - 1,
                       val_header_row = data_row - 1,
                       first_val_col = NULL,
                       gather = FALSE) {

  df = readxl::read_excel(path, sheet,
                          col_types = "text",
                          skip = 0,
                          n_max = data_row - 1,
                          col_names = FALSE)

  if (cat_header_row == val_header_row & is.null(first_val_col)) {
    cat_cols = seq_len(ncol(df))
  } else {
    if (is.null(first_val_col)) {
      first_val_col = which(!is.na(as.character(df[val_header_row, ])))[1]
    }
    cat_cols = 1:(first_val_col - 1)
    val_cols = first_val_col:ncol(df)
  }

  cat_headers = get_headers(df, cat_header_row, cat_cols)
  val_headers = get_headers(df, val_header_row, val_cols,
                            fill = FALSE, make.names = !gather)

  df = readxl::read_excel(path, sheet,
                          col_names = c(cat_headers, val_headers),
                          skip = data_row - 1)

  if (gather) {
    df = tidyr::gather_(df, "Key", "Value", val_headers)
  }

  df

}

