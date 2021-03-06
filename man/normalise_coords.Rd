% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalise_coords.R
\name{normalise_coords}
\alias{normalise_coords}
\title{Normalises spatio-temporal dimensions}
\usage{
normalise_coords(
  data,
  rules = list(lev = c("level"), lat = c("latitude"), lon = c("longitude", "long"),
    time = c("date")),
  extra = list()
)
}
\arguments{
\item{data}{a data.frame}

\item{rules}{a list of "rules". Each element is a possible column name,
which will be renamed using the name of the element of the list.}

\item{extra}{an extra list of rules to add to the default one.}
}
\description{
Normalises spatio-temporal dimensions
}
