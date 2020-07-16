library(metR)
library(magrittr)
library(asymsam)
library(data.table)

indices_file <- data_path("derived", "indices.Rds")

if (!file.exists(indices_file)) {
  hgt <- ReadNetCDF(ERA5(),
                    subset = list(time = c("1979-01-01", "2018-12-31"),
                                  latitude = c(-90, 10)),
                    vars = c(hgt = "z")) %>%
    normalise_coords() %>%
    .[, hgt := hgt/9.8] %>%
    .[, hgt_a := hgt - mean(hgt), by = .(lon, lat, lev, month(time))]

  indices <- hgt[lat <= -20, eof_asym(hgt_a, lon, lat, time, n = 1), by = lev]

  saveRDS(indices, file = data_path("derived", "indices.Rds"))

}

