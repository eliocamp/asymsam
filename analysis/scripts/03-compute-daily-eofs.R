ccf_dt <- function(...) {
  a <- ccf(..., plot = FALSE)
  list(acf = as.vector(a$acf),
       lag = as.vector(a$lag))
}

SAM <- ReadNetCDF(ERA5(),
                  subset = list(time = c("1979-01-01", "2018-12-31"),
                                latitude = c(-90, -20)),
                  vars = c(hgt = "z")) %>%
  normalise_coords() %>%
  .[, hgt := hgt/9.8] %>%
  .[, hgt_a := hgt - mean(hgt), by = .(lon, lat, lev, month(time))] %>%
  .[, hgt := hgt_a*sqrt(cos(lat*pi/180))] %>%
  .[, .(eof = list(eof_flip(EOF(hgt ~ time | lon + lat, n = 1, data = .SD)))),
    by = .(lev)]

sam_sep <- SAM[, eof[[1]]$right, by = .(lev)] %>%
  setnames("hgt", "full") %>%
  .[, c("sym", "asym") := list(mean(full), Anomaly(full)), by = .(lat, lev)]


metadata <- GlanceNetCDF(asymsam::data_path("raw", "ncep.daily.nc"))



make_projection <- function(lev) {
  this_lev <- lev
  day_hgt <- ReadNetCDF(asymsam::data_path("raw", "ncep.daily.nc"),
                        vars = c(hgt = "hgt"),
                        subset = list(lat = c(-90, -20),
                                      level = this_lev,
                                      time = c("1979-01-01", "2018-12-31"))) %>%
    asymsam::normalise_coords() %>%
    .[, month := month(time[1]), by = time]

  day_hgt <- sam_sep[lev == this_lev][day_hgt, on = c("lon", "lat", "lev")]

  day_hgt[, hgt_m := mean(hgt), by = .(lon, lat, month, lev)]
  day_hgt[, hgt_a := hgt - hgt_m]

  day_hgt[, coslat := cos(lat*pi/180)]
  asym <- day_hgt[, .(estimate = sum(hgt_a*asym*coslat)/sum(coslat)), by = .(time, lev)]
  sym <- day_hgt[, .(estimate = sum(hgt_a*sym*coslat)/sum(coslat)), by = .(time, lev)]
  rbind(asym = asym, sym = sym, idcol = "term")
}

levs <- intersect(unique(SAM$lev), metadata$dims$level$vals)

indexes <- lapply(levs, make_projection) %>%
  rbindlist()




lag_max = 10*31
indexes[, estimate_700 := estimate[lev == 700], by = .(term, time)]
indexes[, estimate_50 := estimate[lev == 50], by = .(term, time)]
rbind("700" = indexes[, ccf_dt(x = estimate, y = estimate_700, lag.max = lag_max), by = .(lev, term)],

  "50" =  indexes[, ccf_dt(x = estimate, y = estimate_50, lag.max = lag_max), by = .(lev, term)], idcol = "base_lev") %>%
  .[, lag := lag/31] %>%
  ggplot(aes(lag, lev)) +
  geom_contour_fill(aes(z = acf)) +
  scale_fill_divergent() +
  scale_y_level() +
  scale_x_continuous(breaks = seq(-10, 10)) +
  facet_grid(term~base_lev)

