#' @export
print.nc_file <- function(x, ...) {
   print(metR::GlanceNetCDF(x))
}

#' @param type character indicating the type of data. The resulting base will be type_data
#' @param ... characters to be contactenadted with [file.path()]
#' @export
#' @rdname data_locations
data_path <- function(type = c("raw", "derived"), ...) {
   here::here("analysis", "data", paste0(type[1], "_data"), ...)
}

#' @export
#' @rdname data_locations
AAO <- function() {
   file <- data_path("derived", "aao.Rds")
   checkmate::assert_access(file, access = "r")
   return(file)
}

#' Data locations
#'
#' Functions that return the location of different datasets. They check that
#' relevant file exist and then return it with a class that has a neat print method.
#'
#'
#' @export
#' @rdname data_locations
SST <- function() {
   file <- here::here("DATA", "reanalysis", "ERSST_V5", "sst.ersst.mnmean.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}


#' @export
#' @rdname data_locations
ERA5 <- function() {
   file <- data_path("raw", "era5.z.mon.mean.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}



#' @export
#' @rdname data_locations
OLR <- function() {
   file <- here::here("DATA", "reanalysis", "NOAA", "olr.mon.mean.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}

#' @export
#' @rdname data_locations
CMAP <- function() {
   file <- data_path("raw", "precip.mon.mean.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}

#' @export
#' @rdname data_locations
GPCP <- function() {
   file <- data_path("raw", "gpcp.precip.mon.mean.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}

#' @export
#' @rdname data_locations
GPCC <- function() {
   file <- data_path("raw", "precip.mon.total.v2018.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}


#' @export
#' @rdname data_locations
NOAATemp <- function() {
   file <- data_path("raw", "air.mon.anom.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}

#' @export
#' @rdname data_locations
HadCRUT <- function() {
   file <- data_path("raw", "HadCRUT.4.6.0.0.median.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}


#' @export
#' @rdname data_locations
GISTEMP <- function() {
   file <- data_path("raw", "air.2x2.1200.mon.anom.land.nc")
   checkmate::assert_access(file, access = "r")
   class(file) <- c("nc_file", class(file))
   return(file)
}
