# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:3.6.3

# required
MAINTAINER Elio Campitelli <elio.campitelli@cima.fcen.uba.ar>

RUN R -e "devtools::install_github('r-hub/sysreqs')"  

 # install dependencies (copy only description as to not invalidate cache)

COPY ./system-deps.txt /asymsam/system-deps.txt
RUN R -e "system(paste('sudo apt update && sudo apt install -y ', paste0(readLines('/asymsam/system-deps.txt'), collapse = ' '), collapse = ' '))" 

COPY ./DESCRIPTION /asymsam/DESCRIPTION
RUN R -e "devtools::install_deps('/asymsam', dependencies = TRUE)"

 # render the manuscript 
COPY . /asymsam/
RUN rm -r /asymsam/analysis/paper/paper_cache

RUN R -e "devtools::install('/asymsam', dep = TRUE)" \
    && R -e "rmarkdown::render('/asymsam/analysis/paper/paper.Rmd')"
