% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ReIm.R
\name{ReIm}
\alias{ReIm}
\alias{sep_ReIm}
\title{Splits complex numbers into real and imaginary vectors}
\usage{
ReIm(complex)

sep_ReIm(data, column, longer = TRUE)
}
\arguments{
\item{complex}{vector of complex values}

\item{data}{a data.table}

\item{column}{unquoted name of the column to split into real and imaginary part}

\item{longer}{logical indicating wether to return a "longer" version of the data.frame}
}
\description{
Splits complex numbers into real and imaginary vectors
}
