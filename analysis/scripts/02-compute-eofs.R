library(metR)
library(magrittr)
library(asymsam)
library(data.table)

indices_file <- data_path("derived", "indices.Rds")

message("Hello, you're running the script to compute symmetric and asymmetric SAM.\nI'll be your messenger for the day.")

if (!file.exists(indices_file)) {
  message("Reading data")
  hgt <- ReadNetCDF(ERA5(),
                    subset = list(time = c("1979-01-01", "2018-12-31"),
                                  latitude = c(-90, 10)),
                    vars = c(hgt = "z")) %>%
    normalise_coords() %>%
    .[, hgt := hgt/9.8] %>%
    .[, hgt_a := hgt - mean(hgt), by = .(lon, lat, lev, month(time))]

  message("Computing eofs... (this might take a while, you can go make some coffee).")
  indices <- hgt[lat <= -20, eof_asym(hgt_a, lon, lat, time, n = 1), by = lev]
  message("Done! Saving file.")
  saveRDS(indices, file = data_path("derived", "indices.Rds"))
  fwrite(indices, file = data_path("derived", "indices.csv"), yaml = TRUE)
  message("Everything's ready. Have a nice day!")
} else {
  message("The file ", indices_file, "already exists, so you don't need to do anything. Horray!")
}

