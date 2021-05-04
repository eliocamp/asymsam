#' FFT spectrum
#'
#' A light wrapper around [stats::spec.pgram()] and [stats::ar()]. Returns the spectrum of
#' the signal and the spectrum of the fitted autoregressiv model.
#'
#' @param x numeric vector
#' @param B number of bootstrap samples.
#' @param spans vector of odd integers giving the widths of modified Daniell
#' smoothers to be used to smooth the periodogram
#' @param probs quantile for confidence interval.
#' @param ... other arguments passed to [stats::spec.pgram()]
#'
#' @export
fftspectrum <- function(x, spans = NULL, B = 10000, ..., probs = 0.95) {
  ar_spectrum <- NULL
  mtm <- stats::spec.pgram(x, spans = spans, ..., plot = FALSE)

  out <- data.table::as.data.table(mtm[c("freq", "spec")])

  ar <- ar(stats::ts(x))
  # rho <- a$ar
  # var <- a$var.pred
  out[, ar_spectrum := arspectrum(mtm$freq, ar$ar, ar$var.pred)]
  out[, c(scales::percent(probs)) := null_ar_spectrum(B = B, length(x), ar, spans = spans, ..., probs = probs)]

  return(out[])
}

arspectrum <- function(freq, rho, var) {
  k <- seq_along(rho)
  e <- vapply(freq, function(f) sum(rho * exp(-2*1i*pi*f*k)), complex(1))
  var / (Mod(1 - e))^2
}



null_ar_spectrum_ <- function(B = 100, n, ar, spans = NULL, ..., probs = 0.95) {
  y <- as.vector(stats::arima.sim(model = list(ar = ar$ar), n = n))*sqrt(ar$var.pred)
  nfreq <- length(stats::spec.pgram(y, spans = spans, ..., plot = FALSE)$spec)
  boots <- vapply(seq_len(B), function(b) {
    y <- as.vector(stats::arima.sim(model = list(ar = ar$ar), n = n))*sqrt(ar$var.pred)
    stats::spec.pgram(y, spans = spans, plot = FALSE)$spec

  }, numeric(nfreq))
  data.table::as.data.table(t(apply(boots, 1, stats::quantile, probs = probs)))
}

null_ar_spectrum <- memoise::memoise(null_ar_spectrum_)

null_spec <- memoise::memoise(function(x, spans, B = 1000, ..., probs = 0.95) {

  b <- boot::boot(x, function(d, i) stats::spec.pgram(d[i], spans = spans,
                                                  ...,
                                                  plot = FALSE)$spec,
                  R = B)

  apply(b$t, 2, quantile, probs = probs)
})

