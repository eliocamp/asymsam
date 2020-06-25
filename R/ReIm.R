#' Splits complex numbers into real and imaginary vectors
#'
#' @param complex vector of complex values
#' @param data a data.table
#' @param column unquoted name of the column to split into real and imaginary part
#' @param longer logical indicating wether to return a "longer" version of the data.frame
#'
#' @export
ReIm <- function(complex) {
  list(R = Re(complex), I = Im(complex))
}

#' @export
#' @rdname ReIm
sep_ReIm <- function(data, column, longer = TRUE) {
  R <- part <- I <- NULL
  names <- c("R", "I")
  expr <- quote(data.table::copy(data)[, (names) := ReIm(column)])
  expr  <-  do.call(substitute, list(expr,
                                     list(col = substitute(column))))
  data <- eval(expr)

  if (isTRUE(longer)) {
    data[, deparse(substitute(column)) := NULL]
    data <- data.table::setDT(tidyr::pivot_longer(data, R:I, names_to = "part", values_to = deparse(substitute(column))))
    data[, part := factor(part, levels = c("R", "I"), labels = c("Real", "Imaginary"))]
  }

  return(data[])
}
