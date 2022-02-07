# asymsam


This repository contains data, code and other source files associated with a paper [Assessment of zonally symmetric and asymmetric components of the Southern Annular Mode using a novel approach](https://doi.org/10.1007/s00382-021-05896-5) published in Climate Dynamics. 

## To cite

Campitelli, E., Díaz, L.B. & Vera, C. Assessment of zonally symmetric and asymmetric components of the Southern Annular Mode using a novel approach. Clim Dyn (2021). https://doi.org/10.1007/s00382-021-05896-5

BibTex citation:

```bib

@article{campitelli2021,
  title = {Assessment of Zonally Symmetric and Asymmetric Components of the {{Southern Annular Mode}} Using a Novel Approach},
  author = {Campitelli, Elio and Díaz, Leandro B. and Vera, Carolina},
  date = {2021-08-09},
  journaltitle = {Climate Dynamics},
  shortjournal = {Clim Dyn},
  issn = {1432-0894},
  doi = {10.1007/s00382-021-05896-5},
  url = {https://doi.org/10.1007/s00382-021-05896-5},
  urldate = {2021-08-09}
}

```

## Running the code

Clone this repo. If using RStudio, open the project. 

### Installing system dependencies

System dependencies (on ubuntu) are listd in system-deps.txt. 
To install them you can use 

``` bash
xargs sudo apt-get install -y < system-deps.txt
```

### Installing R packages

This project uses the [renv](https://rstudio.github.io/renv/) package to manage a reproducible environment. If opening this project with RStudio or starting R from the command line from the root directory, renv should automagically install and load itself. 

To recreate the environment then run

```r
renv::restore()
```

This should install all the package dependencies needed to install the package and compile the document. Depending on your operating system, this could take a while. 

Then install this package with

```r
if (!require("devtools")) {
   install.packages("devtools")
}
devtools::install()
```

To get the data needed to reproduce this paper you'll need to set create a user [here](https://cds.climate.copernicus.eu/user/register?destination=/). Once you have your user ready, to to your [user page](https://cds.climate.copernicus.eu/user/) copy your UID and API Key and set them in the environmental variables CDSKEY and CDSUSER. You can do this by creating a file called `.Renviron` on the root folder of this project with 

```
CDSUSER = "xxxx"
CDSKEY = "xxxxxxx-xxxxxx-xxxx-xxx-xxxxxxxx"
```

Note that your USER is **NOT** the user you use to authenticate on the website, but the 5 or 6 digit number you see on the "API key" section on [your user page](https://cds.climate.copernicus.eu/user/)

Then, run `analysis/scripts/01-get-data.R`.

Secondly, run `analysis/scripts/02-compute-eofs.R`. 

Finally, knit the main manuscript located at `analysis/paper/asymsam.Rmd`. 
