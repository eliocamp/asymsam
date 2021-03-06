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
              era5 =  "era5.z.mon.mean.nc",
              era52mt = "era5.2mt.mon.mean.nc")

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


# Download ERA5 temperature -----------------------------------------------
file <- files$era52mt

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
    variable = c("2m_temperature"),
    year = c("1979", "1980", "1981", "1982", "1983", "1984", "1985", "1986",
             "1987", "1988", "1989", "1990", "1991", "1992", "1993", "1994",
             "1995", "1996", "1997", "1998", "1999", "2000", "2001", "2002",
             "2003", "2004", "2005", "2006", "2007", "2008", "2009", "2010",
             "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018"),
    month = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
    time = "00:00",
    grid = c("2.5", "2.5"),
    dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
    target = file
  )


  files <- ecmwfr::wf_request(request = request,
                              user = user,
                              time_out = 5*3600,
                              path = asymsam::data_path())
}




