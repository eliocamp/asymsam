#' Computes symmetric and asymmetric Principal Components
#'
#' Computes EOF of a field (weighted by sqrt(cos(lat))), separates it between
#' its zonally symmetric and zonally asymmetric parts and finally, computes
#' time series of those patterns.
#'
#' @param value value of the variable
#' @param lon,lat,time latitude, longitude and time
#' @param n numeric vector of components to compute.
#'
#' @export
eof_asym <- function(value, lon, lat, time, n = 1) {
  sym <- asym <- full <- PC <- estimate_norm <- term <- estimate <- r.squared <-  partial.r.squared <- adj.r.squared <- NULL
  data <- data.table::data.table(value, lon, lat, time, key = c("lon", "lat", "time"))

  lab_sam <-  c(full = "Full",
                asym = "Asymmetric",
                sym  = "Symmetric")

  eof <- data %>%
    .[, full := value*sqrt(cos(lat*pi/180))] %>%
    metR::EOF(full ~ time | lon + lat, n = n, data = .)
  eof$right[, c("sym", "asym") := list(mean(full), metR::Anomaly(full)), by = .(lat, PC)]

  data[, full := NULL]
  indexes <- eof$right %>%
    data[., on = .NATURAL, allow.cartesian = TRUE]

  pcor <- indexes[, partial_cor(value, sym, asym, weights = cos(lat*pi/180)),
                  keyby = .(time, PC)]

  indexes[, rbind(data.table::as.data.table(lm_lite(value, full, weights = cos(lat*pi/180), r2 = TRUE)),
                  data.table::as.data.table(metR::FitLm(value, sym, asym, weights = cos(lat*pi/180), r2 = TRUE))),
          keyby = .(time, PC)] %>%
    .[term != "(Intercept)"] %>%
    .[, estimate_norm := estimate/stats::sd(estimate[term == "full"]), by = PC] %>%
    .[, term := factor(term, levels = names(lab_sam), ordered = TRUE)] %>%
    .[term != "full", partial.r.squared := pcor$partial_correlation^2] %>%
    .[term == "full", partial.r.squared := r.squared] %>%
    .[, adj.r.squared := NULL] %>%
    .[]
}

#' Flip signs of an EOF
#'
#'
#' @param eof object returned by [metR::EOF()]
#'
#' @export
eof_flip <- function(eof) {
  var <- attr(eof, "value.var", TRUE)
  names <- attr(eof, "names", TRUE)

  eof[[names[1]]][[var]] <- -eof[[names[1]]][[var]]
  eof[[names[2]]][[var]] <- -eof[[names[2]]][[var]]

  return(eof)
}
