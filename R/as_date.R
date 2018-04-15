#' as_date
#' @description Faster alternative to as.Date. Converts character vectors to factors
#' and converts unique values to dates.
#' @param x Character or factor vector of dates.
#' @param format Date format string.
#' @examples
#' as_date("2010-01-01")
#'
#' @export
as_date = function(x, format = "%Y-%m-%d"){

  if (lubridate::is.Date(x)) {
    return(x)
  }

  if (length(x) == 0) {
    return(as.Date(integer(0), origin = "2000-01-01"))
  }


  if (!is.character(x) & !is.factor(x)){
    stop(sprintf('Unsupported input class: %s',
                 paste(class(x), collapse = ", ")))
  }

  x = if (is.character(x)) factor(x) else x

  as.Date(levels(x), format)[x]

}

