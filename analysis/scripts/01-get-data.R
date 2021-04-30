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
              gpcc = "precip.mon.total.v2018.nc",
              gpcp = "gpcp.precip.mon.mean.nc",
              era5 =  "era5.z.mon.mean.nc")

dir.create(asymsam::data_path("raw"), recursive = TRUE, showWarnings = FALSE)
dir.create(asymsam::data_path("derived"), recursive = TRUE, showWarnings = FALSE)

# Download AAO ------------------------------------------------------------
file <- files$aao
aao <- rsoi::download_aao(use_cache = !force, file = asymsam::data_path("raw", file)) %>%
  data.table::as.data.table() %>%
  .[, .(time = lubridate::as_datetime(Date), aao = AAO)]

saveRDS(aao, asymsam::data_path("raw", "aao.Rds"))

# Download QBO
qbourl <- "https://psl.noaa.gov/data/correlation/qbo.data"

qbo <- data.table::fread(qbourl, na.strings = "-999.00", skip = 1, header = FALSE,
             nrows = 73, col.names = c("year", as.character(1:12))) %>%
  data.table::melt(id.var = "year", value.name = "qbo") %>%
  na.omit() %>%
  .[, time := lubridate::make_datetime(year, variable)] %>%
  .[, year := NULL] %>%
  .[, variable := NULL]

saveRDS(qbo, asymsam::data_path("derived", "qbo.Rds"))


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

# Download CMAP -----------------------------------------------------------
url <- "ftp://ftp.cdc.noaa.gov/Datasets/gpcc/full_v2018/precip.mon.total.v2018.nc"
file <- files$gpcc

if (!force && file.exists(asymsam::data_path("raw", file))) {
  message("File already exists.")
} else {
  download.file(url, destfile = asymsam::data_path("raw", file))
}

# Download GPCP -----------------------------------------------------------
url <- "ftp://ftp.cdc.noaa.gov/Datasets/gpcp/precip.mon.mean.nc"
file <- files$gpcp

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

#
# for (y in seq(1979, 2018)) {
#   url <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/pressure/hgt.", y, ".nc")
#   dir.create(asymsam::data_path("raw", "ncep"), )
#   file <- asymsam::data_path("raw", "ncep", paste0("hgt.daily.", y, ".nc"))
#   if (!file.exists(file)) {
#     download.file(url, destfile = file)
#   }
# }
#
# ncepfiles <- list.files(asymsam::data_path("raw", "ncep"), full.names = TRUE)
#
# asymsam::nc_concatenate(ncepfiles, asymsam::data_path("raw", "ncep.daily.nc"), overwrite = TRUE)

# # Antarctic sea ice
# # from https://nsidc.org/data/G02135
# months <- 1:12
# years <- 1979:2019
# dates <- data.table::CJ(years, months)
# # There are no data from 3 December 1987 to 13 January 1988 due to satellite problems. :sob:
# dates <- dates[!(paste0(years, months) %in% c(198712, 19881))]
#
# make_url <- function(year, month) {
#   month_zero <- formatC(month, width = 2, flag = "0")
#   month_Abb <- month.abb[month]
#
#   file_name <- paste0("S_", paste0(year, month_zero), "_concentration_v3.0.tif")
#   url <- paste0("ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/south/monthly/geotiff/",
#                 month_zero, "_", month_Abb, "/", file_name)
#
#   list(file_name,
#        url)
# }
#
# dates[, c("file_name", "url") := make_url(years, months)]
# p <- txtProgressBar(max = 100, style = 3)
#
# end_file <- asymsam::data_path("derived", "ice.Rds")
# if (!file.exists(end_file)) {
#   ices <- lapply(seq_len(nrow(dates)), function(i) {
#     file <- asymsam::data_path("raw", "ice", dates$file_name[i])
#     dir.create(asymsam::data_path("raw", "ice"), showWarnings = FALSE)
#     if (!file.exists(file)) {
#       download.file(dates$url[i], file)
#     }
#     # file
#     ice <- raster::raster(file)
#
#     proj <<- as.character(raster::crs(ice))
#
#     r <- data.table::as.data.table(raster::as.data.frame(ice, xy = TRUE, long = TRUE))
#     r[, layer := NULL]
#     r[, time := lubridate::make_datetime(dates$years[i], dates$months[i])]
#
#
#     no_ice <- c(pole = 2510,
#                 coast = 2530,
#                 land  = 2540,
#                 ocean = 0)
#     # Values from 1-150 (1% to 15% concentration) are statistically irrelevant
#     # because data values less than 15% from passive microwave instruments are too
#     # uncertain to use, so these should be ignored
#     missing <-  c(2550) #
#
#     r <- r[!(value %in% no_ice)]
#     r[value %in% missing, value := NA]
#     r[, value := value/10/100]  # divide by 10 to get percent
#     setTxtProgressBar(p, i/nrow(dates)*100)
#     r[]
#
#   })
#   close(p)
#   ice <- data.table::rbindlist(ices)
#
#
#   ice[, c("lon", "lat") := proj4::project(list(x, y), proj = proj, inverse = TRUE)]
#   ice[, lon := metR::ConvertLongitude(lon)]
#   data.table::setnames(ice, "value", "concentration")
#
#   saveRDS(ice, end_file)
#   saveRDS(proj, asymsam::data_path("derived", "ice_proj.Rds"))
# }
