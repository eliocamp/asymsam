# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.5.3

# required
MAINTAINER Elio Campitelli <elio.campitelli@cima.fcen.uba.ar>



# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  && sudo apt-get update \
  && sudo apt install pandoc pandoc-citeproc libssl-dev zlib1g-dev libxml2-dev libsecret-1-dev libnetcdf-dev make gdal-bin libgeos-dev libgeos++-dev libudunits2-dev libv8-dev imagemagick -y 

RUN sudo apt install libgdal-dev libsodium-dev libjq-dev libprotobuf-dev protobuf-compiler -y 
  
 # install dependencies (copy only description as to not invalidate cache)
COPY ./DESCRIPTION /asymsam/DESCRIPTION
RUN R -e "devtools::install_deps('/asymsam', dependencies = TRUE)"

 # render the manuscript 
COPY . /asymsam/

RUN R -e "devtools::install('/asymsam', dep = TRUE)" \
    && R -e "rmarkdown::render('/asymsam/analysis/paper/paper.Rmd')"
