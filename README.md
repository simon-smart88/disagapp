# disagapp 

<img src="https://raw.githubusercontent.com/simon-smart88/disagapp/master/inst/shiny/www/logo.png" width="259" height="300" align="right" style="border:10px solid white;">

Disagapp is an application written in R that can be used to perform disaggregation regression analyses using [{disaggregation}](https://cran.r-project.org/package=disaggregation). 

Disagapp was built using the [{shinyscholar}](https://github.com/simon-smart88/shinyscholar) template which was itself forked from {wallace} v2.0.5 ([CRAN](https://cran.r-project.org/package=wallace), [website](https://wallaceecomod.github.io/wallace/index.html))

It is currently in pre-alpha.

Install *disagapp* via Github and run the application with the following R code.

```R
install.packages("devtools")
devtools::install_github("simon-smart88/disagapp")
library(disagapp)
run_disagapp()
```

## Components and modules
The application is divided in components that are steps in the analysis and modules that are possible options in each step of the analysis. Each of the modules calls a function of the same name, either in this package or in `{disaggregation}`.

### Response - load the response data
- Combine spreadsheet and shapefile: upload both files and merge them
- Upload spreadsheet: upload the data and combine it with boundary data 
- Upload shapefile: when the data is already merged in a shapefile
- Example datasets: load an example dataset
- Edit data: remove unwanted parts of the data
- Simplify polygons: simplify the geometries of the boundary data

### Covariates - load covariates to use in the analysis
- Accessibility: The time required to travel to cities or healthcare. Provided by the Malaria Atlas Project via `{malariaAtlas}`
- Climate: Various bioclimatic variables relating to temperature and precipitation. Provided by Worldclim via `{geodata}`
- Land use: The percentage of land covered by different classes of land use. Provided by the Copernicus programme
- Nighttime lights: Satellite imagery of the intensity of nighttime lights. Provided by NASA via `{blackmarbler}` developed by the World Bank
- Distance to water: The distance to surface water. Provided by ESRI using data from USGS and ESA
- Upload covariates: Upload your own covariates in the `.tif` format

### Aggregation - load an aggregation raster
- Population density: Population data provided by Worldpop
- Upload aggregation: Upload your own aggregation raster in the `.tif` format
- Uniform: Generate a uniform aggregation raster

### Prepare - prepare the data for modelling
- Generate mesh: generate a spatial mesh from the response data
- Summarise and resample covariates: resample covariates so that they have the same resolution and extent
- Scale covariates: scales the covariates so that their coefficients can be compared fairly 
- Covariate correlations: examine correlations between covariates 
- Reduce covariate resolution: generate lower resolution covariates to speed up model fitting
- Finalise data preparation: combine all the data together

### Fit - fit a disaggregation model
- Fit model: using either gaussian, binomial or poisson likelihood functions and logit, log or identity link functions

### Predict - make predictions
- Make predictions: generate predictions from the model
- Transfer predictions: transfer predictions to a new area of interest

### *Reproduce*  
- Download session code: download a `.Rmd` file that completely replicates the analysis
- Reproduce environment: use `{renv}` to capture dependencies, allowing the analysis to be reproduced exactly
- Download covariates: download copies of the covariate data
- Download package references: download a list of all the packages used in the analysis
