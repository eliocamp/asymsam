#' Normalises spatio-temporal dimensions
#'
#' @param data a data.frame
#' @param rules a list of "rules". Each element is a possible column name,
#' which will be renamed using the name of the element of the list.
#' @param extra an extra list of rules to add to the default one.
#'
#' @export
normalise_coords <- function(data,
                             rules =  list(lev = c("level"),
                                           lat = c("latitude"),
                                           lon = c("longitude", "long"),
                                           time = c("date")),
                             extra = list()) {

  checkmate::assert_list(rules, types = "character", names = "named", any.missing = FALSE)
  checkmate::assert_list(extra, types = "character", names = "named", any.missing = FALSE)

  rules <- c(rules, extra)

  for (f in seq_along(rules)) {
    old <- colnames(data)[colnames(data) %in% rules[[f]]]

    if (length(old) != 0) {
      data.table::setnames(data,
                           old,
                           names(rules)[[f]], skip_absent = TRUE)
    }
  }
  return(invisible(data))
}
