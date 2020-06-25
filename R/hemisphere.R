#' Convert latitude into hemispheres
#'
#' @param lat latitude
#'
#' @export
hemisphere <- function(lat) {
  factor(ifelse(lat < 0, "sh", "nh"), levels = c("nh", "sh"),
         labels = c("Northern Hemisphere",
                    "Southern Hemisphere"),
         ordered = TRUE)
}
