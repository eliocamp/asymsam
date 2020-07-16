## Downloads data from the oficial channels and
##  - AAO from NOAA's Climate Prediction Centre (see ?rsoi::download_aao)
##  - GISTEMP from NOAA's Physical Sciences Laboratory (https://psl.noaa.gov/data/gridded/data.gisstemp.html)
##  - CMAP from NOAA's Physical Sciences Laboratory (https://psl.noaa.gov/data/gridded/data.cmap.html)
##  - ERA5 Geopotential height from Climate Data Store (https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-pressure-levels-monthly-means?tab=overview)
##

library(magrittr)

force <- FALSE

files <- list(aao = "aao.csv",
              gistemp = "air.2x2.1200.mon.anom.land.nc",
              NOAAGlobalTemp = "air.mon.anom.nc",
              cmap = "precip.mon.mean.nc",
              hadcrut = "HadCRUT.4.6.0.0.median.nc",
              era5 =  "era5.z.mon.mean.nc")

dir.create(asymsam::data_path("raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(asymsam::data_path("derived"), recursive = TRUE, showWarnings = FALSE)

# Download AAO ------------------------------------------------------------
file <- files$aao
aao <- rsoi::download_aao(use_cache = !force, file = asymsam::data_path("raw", file)) %>%
  data.table::as.data.table() %>%
  .[, .(time = lubridate::as_datetime(Date), aao = AAO)]

saveRDS(aao, asymsam::data_path("derived", "aao.Rds"))

# Download GISTEMP --------------------------------------------------------
url <- "ftp://ftp.cdc.noaa.gov/Datasets/gistemp/landonly/1200km/air.2x2.1200.mon.anom.land.nc"
file <- files$gistemp

if (!force && file.exists(asymsam::data_path("raw", file))) {
  message("File already exists.")
} else {
  download.file(url, destfile = asymsam::data_path("raw", file))
}

# Download NOAAGlobalTemp --------------------------------------------------------
url <- "ftp://ftp.cdc.noaa.gov/Datasets/noaaglobaltemp/air.mon.anom.nc"
file <- files$NOAAGlobalTemp

if (!force && file.exists(asymsam::data_path("raw", file))) {
  message("File already exists.")
} else {
  download.file(url, destfile = asymsam::data_path("raw", file))
}

# Download HadCRUT4 --------------------------------------------------------
url <- "https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT.4.6.0.0.median.nc"
file <- files$hadcrut

if (!force && file.exists(asymsam::data_path("raw", file))) {
  message("File already exists.")
} else {
  download.file(url, destfile = asymsam::data_path("raw", file))
}


# Download CMAP -----------------------------------------------------------
url <- "ftp://ftp.cdc.noaa.gov/Datasets/cmap/std/precip.mon.mean.nc"
file <- files$cmap

if (!force && file.exists(asymsam::data_path("raw", file))) {
  message("File already exists.")
} else {
  download.file(url, destfile = asymsam::data_path("raw", file))
}

# Download ERA5 -----------------------------------------------------------
user <- Sys.getenv("CDSUSER", NA)
key <- Sys.getenv("CDSKEY", NA)
file <- files$era5

if (!force && file.exists(asymsam::data_path("raw", file))) {
  message("File already exists.")
} else {
  if (is.na(user) | is.na(key)) {
    stop("Please set the environmental variables CDSUSER and CDSKEY")
  }

  ecmwfr::wf_set_key(user = user,
                     key = key,
                     service = "cds")

  request <- list(
    format = "netcdf",
    product_type = "monthly_averaged_reanalysis",
    variable = "geopotential",
    pressure_level = c("1", "2", "3", "5", "7", "10", "20", "30", "50",
                       "70", "100", "125", "150", "175", "200", "225",
                       "250", "300", "350", "400", "450", "500", "550",
                       "600", "650", "700", "750", "775", "800", "825",
                       "850", "875", "900", "925", "950", "975", "1000"),
    year = as.character(1979:2019),
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    time = "00:00",
    grid = c("2.5", "2.5"),
    dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means",
    target = file
  )

  files <- ecmwfr::wf_request(request = request,
                              user = user,
                              time_out = 5*3600,
                              path = asymsam::data_path())
}


for (y in seq(1979, 2018)) {
  url <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/hgt.", y, ".nc")
  dir.create(asymsam::data_path("raw", "ncep"), )
  file <- asymsam::data_path("raw", "ncep", paste0("hgt.daily.", y, ".nc"))
  if (!file.exists(file)) {
    download.file(url, destfile = file)
  }
}

ncepfiles <- list.files(asymsam::data_path("raw", "ncep"), full.names = TRUE)

asymsam::nc_concatenate(ncepfiles, asymsam::data_path("raw", "ncep.daily.nc"), overwrite = TRUE)
