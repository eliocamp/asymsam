library(data.table)
library(metR)
library(magrittr)
library(ggplot2)
D <- `[`

# Esto solo funciona en vegeta :(
cmip_folder <- "/pikachu/datos3/CMIP6/historical/mon/zg/"
files <- list.files(cmip_folder)

simulaciones <- unglue::unglue_data(files, "zg_Amon_{model}_historical_i{init}p{physics}f{forcing}_{grid}_{start_date}-{end_date}.nc4") %>% 
   as.data.table() %>% 
   D(, file := files) %>% 
   D(, members := GlanceNetCDF(file.path(cmip_folder, file))$dims$ensemble$len, by = file) %>% 
   D(, .SD[1], by = model)


compute_asymsam <- function(file, member = 1) {
   nc_con <- ncdf4::nc_open(file)
   metadata <- GlanceNetCDF(nc_con)
   plevs <- metadata$dims$plev$vals
   
   sam <- lapply(plevs, function(plev) {
      data <- ReadNetCDF(nc_con, vars = c(hgt = "zg"), 
                         subset = list(lat = c(-90, -20), 
                                       plev = plev,
                                       time = c("1979-01-01", NA),
                                       ensemble = member),
                         out = "array")[[1]]
      dims <- attr(data, "dimvalues")
      
      time_lookup <- data.table(true_time = dims$time,
                                time = dimnames(data)[["time"]])
      
      months <- data.table::month(dims$time)
      
      month_anomaly <- function(x) {
         x - ave(x, months)
      }
      
      data <- apply(data, c("lon", "lat", "plev", "ensemble"), month_anomaly)
      
      data <- aperm(data, c("time", "ensemble", "lon", "lat", "plev"))
      dim(data) <- list(prod(lengths(dimnames(data))[1:2]),
                        prod(lengths(dimnames(data))[3:5]))
      nas <- is.na(data)
      data[nas] <- 0   # Some models have NA on lower levels ¯\_(ツ)_/¯
      
      svd <- irlba::irlba(data, 1, 1, rng = runif)   
      sam <- svd$v[, 1]
      
      dim(sam) <- lengths(dims[c("lon", "lat", "plev")])
      dimnames(sam) <- dims[c("lon", "lat", "plev")]
      
      dim(data) <- lengths(dims[c("time", "ensemble", "lon", "lat", "plev")])
      dimnames(data) <- dims[c("time", "ensemble", "lon", "lat", "plev")]
      
      asym <- apply(sam, c("lat", "plev"), Anomaly)
      sym <- apply(sam, c("lat", "plev"), function(x) rep(mean(x), length(x)))
      weights <- rep(cos(dims$lat*pi/180), each = length(dims$lon))
      
      
      asymsam <- apply(data, c("time", "ensemble"), function(A) {
         r_sam  <- FitLm(c(A),  sam = c(sam), weights = weights, r2 = TRUE)
         r_sym  <- FitLm(c(A),  sym = c(sym), weights = weights, r2 = TRUE)
         r_asym <- FitLm(c(A), asym = c(asym), weights = weights, r2 = TRUE)
         
         c(r_sam$estimate[2], r_sam$r.squared[2],
           r_sym$estimate[2], r_sym$r.squared[2],
           r_asym$estimate[2], r_asym$r.squared[2])
      })
      
      dim(asymsam) <- c(variable  = 2, index = 3, lengths(dims[ c("time", "ensemble", "plev")]))
      dimnames(asymsam) <- c(list(variable = c("estimate", "r2"),
                                  sam = c("sam", "sym", "asym")),
                             dims[ c("time", "ensemble", "plev")])
      
      time_series <- data.table::as.data.table(reshape2::melt(asymsam, as.is = TRUE))
      time_series <- time_lookup[time_series, on = "time"][, time := NULL]
      setnames(time_series, "true_time", "time")
      time_series[, plev := as.integer(plev)]
      time_series <- data.table::dcast(time_series, time + sam + plev + ensemble ~ variable)
      
      
      fields <- reshape2::melt(sam) %>% 
         as.data.table() %>% 
         .[, ":="(lat = as.numeric(lat), 
                  lon = as.numeric(lon),
                  plev = as.integer(plev))] 
      
      list(indices = time_series,
           fields = fields)
      
   })
   
   fields <- rbindlist(
      lapply(sam, function(s) {
         s$fields
      }))
   
   indices <- rbindlist(
      lapply(sam, function(s) {
         s$indices
      }))
   list(fields = list(fields), 
        indices = list(indices))
}

sam_rds <- here::here("analysis", "cmip6", "sam_cmip.Rds")
if (file.exists(sam_rds)) {
   sam <- readRDS(sam_rds)
} else {
   message("Get a cup o' coffee.")
   sam <- simulaciones[, compute_asymsam(file.path(cmip_folder, file), c(1, NA)), by = model]   
}



sam[, indices[[1]], by = model] %>% 
   .[, FitLm(r2, time), by = .(sam, plev, ensemble, model, season(time))] %>% 
   .[sam == "asym"] %>% 
   .[term == "time"] %>% 
   ggplot(aes(plev/100, estimate)) +
   geom_line(aes(group = ensemble)) +
   facet_grid(season ~ model) +
   scale_x_level() +
   coord_flip()



sam[, fields[[1]], by = model] %>% 
   .[plev == 20000] %>% 
   ggplot(aes(lon, lat)) +
   geom_contour_fill(aes(z = value)) +
   facet_wrap(~model)
   
sam[, fields[[1]], by = model] %>% 
   .[lat < - 80] %>% 
   .[, mean(value), by = .(model, plev)] %>% 
   ggplot(aes(plev, V1)) +
   geom_line(aes(color = model)) +
   geom_point()

A <- 1 - 0.75
   
sam[, indices[[1]], by = model] %>% 
   .[season(time) == "DJF"] %>% 
   .[, estimate := estimate/sd(estimate), by = .(plev, sam, model)] %>%
   .[, FitLm(estimate, time = as.numeric(time)/(3600*24*365), se = TRUE), by = .(sam, plev, ensemble, model, season(time))] %>% 
   # .[sam == "asym"] %>%
   # .[model == "CanESM5"] %>% 
   .[term == "time"] %>% 
   .[, n := uniqueN(ensemble), by = .(model)] %>% 
   .[, t := qt(.975, df)] %>% 
   ggplot(aes(plev/100, estimate)) +
   geom_ribbon(aes(ymin = estimate - std.error*t, ymax = estimate + std.error*t, 
                   group = ensemble, alpha = 1 - A^(1/n))) +
   geom_line(data = ~.x[, .(estimate = mean(estimate)), by = .(model, sam, plev)]) +
   facet_grid(model ~ sam) +
   scale_alpha_identity() +
   scale_x_level() +
   coord_flip()
   

sam[, indices[[1]], by = model] %>% 
   dcast(model + time + plev + ensemble ~ sam, value.var = "estimate") %>% 
   .[, sym - sam] %>% 
   plot()
