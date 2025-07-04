# disagapp v0.0.1

<img src="https://raw.githubusercontent.com/simon-smart88/disagapp/master/inst/shiny/www/logo.png" width="259" height="300" align="right" style="border:10px solid white;">

Disagapp is an application written in R that can be used to perform disaggregation regression analyses using [{disaggregation}](https://cran.r-project.org/package=disaggregation) as described in [Nandi et al. (2023)](https://doi.org/10.18637/jss.v106.i11). A version is deployed to https://disagapp.le.ac.uk/ 

<img src="https://raw.githubusercontent.com/simon-smart88/disagapp/master/screenshot.png" width="600" align="center" style="border:10px solid white;">

Disagapp was built using the [{shinyscholar}](https://github.com/simon-smart88/shinyscholar) template which was itself forked from {wallace} v2.0.5 ([CRAN](https://cran.r-project.org/package=wallace), [website](https://wallaceecomod.github.io/wallace/index.html))

Install *disagapp* via Github and run the application with the following R code.

```R
install.packages("devtools")
devtools::install_github("simon-smart88/disagapp")
library(disagapp)
run_disagapp()
```

## Components and modules
The application is divided into components that are steps in the analysis and modules that are possible options in each step of the analysis. Each of the modules calls a function of the same name, either in this package or in `{disaggregation}`.

### Response - load the response data (`resp`)
- Combine spreadsheet and shapefile: upload both files and merge them
- Upload spreadsheet: upload the data and combine it with boundary data 
- Upload shapefile: when the data is already merged in a shapefile
- Example datasets: load an example dataset
- Edit data: remove unwanted parts of the data
- Simplify polygons: simplify the geometries of the boundary data

### Covariates - load covariates to use in the analysis (`cov`)
- Accessibility: The time required to travel to cities or healthcare. Provided by the Malaria Atlas Project via `{malariaAtlas}`
- Climate: Various bioclimatic variables relating to temperature and precipitation. Provided by Worldclim via `{geodata}`
- Land use: The percentage of land covered by different classes of land use. Provided by the Copernicus programme
- Nighttime lights: Satellite imagery of the intensity of nighttime lights. Provided by NASA via `{blackmarbler}` developed by the World Bank
- Distance to water: The distance to surface water. Provided by ESRI using data from USGS and ESA
- Population density: Population density provided by Worldpop
- Upload covariates: Upload your own covariates in the `.tif` format

### Aggregation - load an aggregation raster (`agg`)
- Population count: Population counts provided by Worldpop
- Land use: The percentage of land covered by different classes of land use. Provided by the Copernicus programme
- Upload aggregation: Upload your own aggregation raster in the `.tif` format
- Uniform: Generate a uniform aggregation raster

### Prepare - prepare the data for modelling (`prep`)
- Generate mesh: generate a spatial mesh from the response data
- Summarise and resample covariates: resample covariates so that they have the same resolution and extent
- Scale covariates: scales the covariates so that their coefficients can be compared fairly 
- Covariate correlations: examine correlations between covariates 
- Reduce covariate resolution: generate lower resolution covariates to speed up model fitting
- Finalise data preparation: combine all the data together

### Fit - fit a disaggregation model (`fit`)
- Fit model: using either gaussian, binomial or poisson likelihood functions and logit, log or identity link functions

### Predict - make predictions (`pred`)
- Make predictions: generate predictions from the model
- Transfer predictions: transfer predictions to a new area of interest

### Reproduce - reproduce the analysis (`rep`)
- Download session code: download a `.Rmd` file that completely replicates the analysis
- Reproduce environment: use `{renv}` to capture dependencies, allowing the analysis to be reproduced exactly
- Download covariates: download copies of the covariate data
- Download package references: download a list of all the packages used in the analysis

## Adding new modules
To add new modules, see the instructions in `adding_modules.md`

## Updating deployments

To update deployments running on shinyserver it is necessary to update the package and also copy the contents of `/inst` into the `/apps` directory.

### Install the updated version package

```bash
R -e "remotes::install_github('simon-smart88/disagapp')"
```

### Update the app directory

Because the repository contains the package files as well as the app, we need to update the local copy of the package and then update the app files.

```bash
cd /local/disagapp
git pull
cp -rf inst/shiny/. /local/shiny/apps/disagapp
```

### Restart shinyserver

```bash
sudo systemctl restart shiny-server
```
