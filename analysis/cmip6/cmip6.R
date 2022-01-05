library(data.table)
library(metR)
library(magrittr)
library(ggplot2)
library(asymsam)
library(tagger)
D <- `[`

theme_set(theme_asymsam(base_size = 12) + theme(tagger.panel.tag.text = element_text(size = 8)))

panel_background <- theme(panel.background = element_rect(fill = "#fbfbfb", color = NA),
                          panel.ontop = FALSE,
                          tagger.panel.tag.background = ggplot2::element_rect(color = NA,
                                                                              fill = "#fbfbfb"))

no_grid <- theme(panel.grid = element_blank())

lab_sam <-  c(sam = "Full",
              asym = "Asymmetric",
              sym  = "Symmetric")
geom_contour_tanaka2 <- purrr::partial(geom_contour_tanaka, range = c(0.01, 0.3))

# salida dput de pikachu
miembros <- structure(list(model = c("CanESM5", "CNRM-CM6-1", "CNRM-ESM2-1",
                         "GISS-E2-1-G", "IPSL-CM6A-LR", "MIROC6", "MRI-ESM2-0"),
                         members = c(25L, 10L, 5L, 10L, 32L, 50L, 5L)),
                      row.names = c(NA, -7L), class = c("data.table", "data.frame"))

geom_coords <- function() {
  list(
    geom_segment(data = data.frame(xstart =  seq(0, 360 - 30, by = 30),
                                   xend =  seq(0, 360 - 30, by = 30),
                                   ystart = -90 + 15,
                                   yend = Inf),
                 aes(x = xstart, xend = xend, y = ystart, yend = yend),
                 size = 0.1, alpha = 0.5),

    geom_hline(yintercept = seq(-90, 0, by = 15), size = 0.1, alpha = 0.5),
    shadowtext::geom_shadowtext(data = data.frame(x = 0, y = seq(-90 + 15, 0, by = 15)),
                                aes(x, y, label = LatLabel(y)), size = 1.5, alpha = 0.7,
                                colour = "black",
                                bg.colour = "white" )
  )
}


sam_rds <- here::here("analysis", "cmip6", "sam_cmip.Rds")
if (file.exists(sam_rds)) {
  sam <- readRDS(sam_rds)
} else {
  message("Get a cup o' coffee.")

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

  sam <- simulaciones[, compute_asymsam(file.path(cmip_folder, file), c(1, NA)), by = model]
}

indices_file <- data_path("derived", "indices.Rds")

indices <- readRDS(indices_file)
indices[term == "full", term := "sam"]




r2 <- sam[, fields[[1]], by = model] %>%
  copy() %>%
  .[plev == 70000] %>%
  .[, ":="(value_m = mean(value),
           value_a = value - mean(value)), by = .(model, plev, lat)] %>%

  .[, FitLm(value, value_a, r2 = TRUE), by = .(model, plev)] %>%
  rm_intercept() %>%
  .[term == "value_a"] %>%
  .[, .(model, r.squared)] %>%
  .[miembros, on = "model"]


sam[, fields[[1]], by = model] %>%
  copy() %>%
  .[plev == 70000] %>%
  .[lat < -67 & abs(value) < 1e-5, value := NA] %>%

  .[, value := value/sd(value, na.rm = TRUE), by = .(model, plev)] %>%
  .[r2, on = "model"] %>%
  .[, model := paste0(model, " — n: ", members, "\n",
                      "(", scales::percent(r.squared, accuracy = 0.1), ")")] %>%
  .[, model := reorder(model, -r.squared)] %>%

  ggplot(aes(lon, lat)) +
  geom_contour_fill(aes(z = value),
                    breaks = AnchorBreaks(0, exclude =  0),
                    global.breaks = FALSE) +
  geom_contour_tanaka2(aes(z = value),
                       breaks = AnchorBreaks(0, exclude =  0),
                       size = 0.3,
                       global.breaks = FALSE) +
  # geom_contour2(aes(z = env)) +
  geom_qmap(~.x[lat <= -20]) +
  geom_coords() +
  scale_x_longitude(labels = NULL) +
  scale_y_latitude(limits = c(NA, -20), labels = NULL) +
  scale_fill_divergent(guide = "none") +
  # scale_fill_divergent(NULL,
  #                      guide = "none",
  #                      # guide = guide_colorsteps_bottom(even.steps = TRUE),
  #                      limits = c(-45, 45),
  #                      oob = scales::squish,
  #                      breaks = AnchorBreaks(0, 10, 0)(c(-45, 45))) +
  scale_linetype(guide = "none") +
  coord_polar() +
  facet_wrap(~model, nrow = 2) +
  # tag_facets("cr", position = list(x = 0.15, y = 0.85)) +
  no_grid


selected_models <- r2[order(-r.squared)] %>%
  .[c(1:3, .N)]


obs_trends <- indices %>%
  copy() %>%
  .[, lapply(.SD, mean), by = .(lev, PC, term, time = seasonally(time))] %>%
  .[, season := season(time)] %>%
  .[, estimate_norm := -estimate_norm/sd(estimate_norm), by = .(lev, term)] %>%
  .[, FitLm(estimate_norm, time = time, se = TRUE), by = .(type = term, lev, season)] %>%
  rm_intercept() %>%
  .[, term := NULL] %>%
  .[, estimate := estimate*3600*24*365*10] %>%
  .[, std.error := std.error*3600*24*365*10] %>%
  .[, pval := Pvaluate(estimate, std.error, df, "fdr")] %>%
  setnames("type", "sam") %>%
  .[, t := qt(.975, df)]

models_trends <- sam[, indices[[1]], by = model] %>%
  .[, sam := factor(sam, levels = c("sam", "asym", "sym"))] %>%
  .[season(time) == "DJF"] %>%
  .[, .(estimate = mean(estimate)), by = .(time = seasonally(time), ensemble, model, plev, sam)] %>%
  .[, estimate := -estimate/sd(estimate), by = .(plev, sam, model)] %>%
  .[, FitLm(estimate, time = as.numeric(time)/(3600*24*365*10), se = TRUE), by = .(sam, plev, ensemble, model, season(time))] %>%
  # .[sam == "sam"] %>%
  # .[model == "CanESM5"] %>%
  .[term == "time"] %>%
  .[, n := uniqueN(ensemble), by = .(model)] %>%
  .[, t := qt(.975, df)]

models_trends %>%
  .[selected_models, on = "model"] %>%
  .[, model := paste0(model, " — n: ", members, "\n",
                      "(", scales::percent(i.r.squared, accuracy = 0.1), ")")] %>%
  .[, model := reorder(model, -i.r.squared)] %>%
  ggplot(aes(plev/100, estimate)) +

  geom_hline(yintercept = 0) +
  geom_line(aes(group = ensemble), alpha = 0.2) +
  # geom_ribbon(aes(ymax = estimate + std.error*t, ymin = estimate - std.error*t,
  #                 group = ensemble, alpha = 1 - A^(1/n)),
  #             fill = "black",
  #             size = 0.1) +

  geom_ribbon(data = ~.x[, .(se = sd(estimate),
                             t = qt(.975, .N),
                             estimate = mean(estimate)), by = .(model, sam, plev)],
              aes(ymin = estimate - se,
                  ymax = estimate + se),
              fill = "#95a3ab", color = "black",
              alpha = .6, size = 0.1) +
  geom_line(data = ~.x[, .(estimate = mean(estimate)), by = .(model, sam, plev)]) +

  geom_line(data = obs_trends[season == "DJF"], aes(lev, estimate),
            color = "#a10705") +

  facet_grid(model ~ sam, labeller = labeller(sam = lab_sam)) +
  scale_alpha_identity() +
  scale_x_level() +
  scale_y_continuous("Standard deviations per decade") +
  coord_flip() +
  # tag_facets("cr") +
  panel_background +
  theme(strip.text.y = element_text(size = rel(0.7)))







sam[, indices[[1]], by = model] %>%
  .[, mean(r2), by = .(plev, sam, model)] %>%
  .[sam != "sam"] %>%
  ggplot(aes(plev/100, V1)) +
  geom_line(aes(color = sam)) +
  geom_line(data = indices %>%
              .[term != "sam"] %>%
              .[, mean(r.squared), by = .(sam = term, lev)],
            aes(lev, V1, color = sam), linetype = 3
  ) +
  scale_x_level() +
  facet_wrap(~model) +
  coord_flip()



obs_trends_r2 <- indices %>%
  copy() %>%
  .[, lapply(.SD, mean), by = .(lev, PC, term, time = seasonally(time))] %>%
  .[, season := season(time)] %>%
  # .[, estimate_norm := -estimate_norm/sd(estimate_norm), by = .(lev, term)] %>%
  .[, FitLm(r.squared, time = time, se = TRUE), by = .(type = term, lev, season)] %>%
  rm_intercept() %>%
  .[, term := NULL] %>%
  .[, estimate := estimate*3600*24*365*10] %>%
  .[, std.error := std.error*3600*24*365*10] %>%
  .[, pval := Pvaluate(estimate, std.error, df, "fdr")] %>%
  setnames("type", "sam") %>%
  .[, t := qt(.975, df)]


sam[, indices[[1]], by = model] %>%
  .[season(time) == "DJF"] %>%
  .[sam != "sam"] %>%
  .[sam == "asym"] %>%
  .[, FitLm(r2, time = as.numeric(time)/(3600*24*365*10), se = TRUE), by = .(sam, plev, ensemble, model, season(time))] %>%
  # .[sam == "asym"] %>%
  # .[model == "CanESM5"] %>%
  .[term == "time"] %>%
  .[, n := uniqueN(ensemble), by = .(model)] %>%
  .[, t := qt(.975, df)] %>%
  .[selected_models, on = "model"] %>%
  .[, model := paste0(model, " — n: ", members, "\n",
                      "(", scales::percent(i.r.squared, accuracy = 0.1), ")")] %>%
  .[, model := reorder(model, -i.r.squared)] %>%
  ggplot(aes(plev/100, estimate)) +

  geom_hline(yintercept = 0) +
  geom_line(aes(group = ensemble), alpha = 0.2) +
  # geom_ribbon(aes(ymax = estimate + std.error*t, ymin = estimate - std.error*t,
  #                 group = ensemble, alpha = 1 - A^(1/n)),
  #             fill = "black",
  #             size = 0.1) +

  geom_ribbon(data = ~.x[, .(se = sd(estimate),
                             t = qt(.975, .N),
                             estimate = mean(estimate)), by = .(model, sam, plev)],
              aes(ymin = estimate - se,
                  ymax = estimate + se),
              fill = "#95a3ab", color = "black",
              alpha = .6, size = 0.1) +
  geom_line(data = ~.x[, .(estimate = mean(estimate)), by = .(model, sam, plev)]) +

  geom_line(data = obs_trends_r2[season == "DJF"][sam == "asym"], aes(lev, estimate),
            color = "#a10705") +

  facet_wrap(model ~ ., labeller = labeller(sam = lab_sam)) +
  scale_alpha_identity() +
  scale_x_level() +
  scale_y_continuous("Change in explained variance per decade",
                     label = scales::percent_format(1)) +
  coord_flip() +
  # tag_facets("cr") +
  panel_background
