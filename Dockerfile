# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.5.3

# required
MAINTAINER Elio Campitelli <elio.campitelli@cima.fcen.uba.ar>

COPY . /asymsam

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  && sudo apt-get install libnetcdf-dev netcdf-bin libudunits2-dev \
  # build this compendium package
  && R -e "devtools::install('/asymsam', dep=TRUE)" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main Rmd file
  && R -e "rmarkdown::render('/asymsam/analysis/paper/paper.Rmd')"
