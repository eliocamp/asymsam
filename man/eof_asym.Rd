% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eof_asym.R
\name{eof_asym}
\alias{eof_asym}
\title{Computes symmetric and asymmetric Principal Components}
\usage{
eof_asym(value, lon, lat, time, n = 1)
}
\arguments{
\item{value}{value of the variable}

\item{lon, lat, time}{latitude, longitude and time}

\item{n}{numeric vector of components to compute.}
}
\description{
Computes EOF of a field (weighted by sqrt(cos(lat))), separates it between
its zonally symmetric and zonally asymmetric parts and finally, computes
time series of those patterns.
}
