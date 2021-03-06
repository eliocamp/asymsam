% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maps.R
\name{geom_qmap}
\alias{geom_qmap}
\title{Quick maps}
\usage{
geom_qmap(
  subset = identity,
  color = "gray50",
  size = 0.2,
  fill = NA,
  wrap = c(0, 360),
  weighting = 0.7,
  keep = 0.015,
  ...
)
}
\arguments{
\item{subset}{a function that will be applied to the map data.}

\item{color, size, fill}{parameters passed to \code{\link[ggplot2:geom_polygon]{ggplot2::geom_polygon()}}}

\item{wrap}{vector of length 2 interpreted as the longitude range of a global map (see \code{\link[maps:map]{maps::map()}})}

\item{weighting}{weighting parameter passsed to \code{\link[rmapshaper:ms_simplify]{rmapshaper::ms_simplify()}}}

\item{keep}{proportions of point to retain (passed to \code{\link[rmapshaper:ms_simplify]{rmapshaper::ms_simplify()}})}

\item{...}{other arguments passed to \code{\link[ggplot2:geom_polygon]{ggplot2::geom_polygon()}}}
}
\description{
Quick maps
}
