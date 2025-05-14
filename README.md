# disagapp v0.0.1

<img src="https://raw.githubusercontent.com/simon-smart88/disagapp/master/inst/shiny/www/logo.png" width="259" height="300" align="right" style="border:10px solid white;">

Disagapp is an application written in R that can be used to perform disaggregation regression analyses using [{disaggregation}](https://cran.r-project.org/package=disaggregation). A version is deployed to https://disagapp.le.ac.uk/ 

Disagapp was built using the [{shinyscholar}](https://github.com/simon-smart88/shinyscholar) template which was itself forked from {wallace} v2.0.5 ([CRAN](https://cran.r-project.org/package=wallace), [website](https://wallaceecomod.github.io/wallace/index.html))

Install *disagapp* via Github and run the application with the following R code.

```R
install.packages("devtools")
devtools::install_github("simon-smart88/disagapp")
library(disagapp)
run_disagapp()
```

## Components and modules
The application is divided in components that are steps in the analysis and modules that are possible options in each step of the analysis. Each of the modules calls a function of the same name, either in this package or in `{disaggregation}`.

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

- This guide details how to develop a module from scratch, but depending on what the new module does, it may be easier to copy an existing module instead and use this as a guide to refactoring it.
- Install shinyscholar with `devtools::install_github("simon-smart88/shinyscholar")`
- Clone the repository using `git clone https://github.com/simon-smart88/disagapp`

### Create module files

- Determine which component the module will be added to - the shorthand in the section titles above forms the first part of the module's identifier.
- Use `shinyscholar::create_module()` to create the module files:
  - `id` is the module identifier formed from the component identifier (listed above) and a single word identifier for the module. We will use `fit_improved` as the identifier in this example.
  - `dir` is the root directory of the application.
  - `map` should be set to `TRUE` if the results of the module need to be plotted on the map.
  - `result` should be set to `TRUE` if the module produces graphs to be presented in the Results tab.
  - `rmd` should be set to `TRUE` if the module should be included in the reproducible Rmarkdown.
  - `save` should be set to `TRUE` if the module has inputs that should be saved and loaded when the app is saved.
  - `download` should be set to `TRUE` if the module will produce an output that you want the user to be able to download.
  - `async` should be set to `TRUE` if the the module takes more than a few seconds to run so that it operates asynchronously. 
  - `init` should be left set as `FALSE`.
  - e.g. assuming you are in the root directory run: 

```R
shinyscholar::create_module(
  id = "fit_improved", 
  dir = ".", 
  map = FALSE,
  result = TRUE,
  rmd = TRUE,
  save = TRUE,
  download = TRUE,
  async = TRUE)`
```

### Developing the function

- Develop the new functionality outside of Shiny first e.g. in an Rmarkdown file using example data.
- Open `fit_improved_f.R` in the `R/` directory and copy the code you have developed into the `fit_improved` function.
- If the function will run synchronously:
  - Add a `logger` parameter as the last parameter that is set to `NULL` by default.
- If the function will run asynchronously:
  - The function will only run asynchronously inside the app, not when used in the Rmarkdown, so the function needs to be flexible to both use cases.
  - Add an `async` parameter as the last parameter that is set to `FALSE` by default. 
  - Any rasters used inside the function need to be wrapped before they are passed to the function, unwrapped inside it, wrapped if they are to be returned and then unwrapped inside the app. 
  Use `if (async)` blocks to unwrap rasters using `terra::unwrap()` at the start of the function and wrap them at the end using `terra::wrap()`
  - If there are any messages that you want to pass to the logger when the function runs successfully, these should be stored in a `list()` along with the main output
- Add checks at the start of the function that check that inputs are in the correct format and pass the errors to `writeLog` or `asyncLog` depending on whether the function is synchronous or asynchronous respectively e.g.:

```R
  if (!inherits(shape, "sf")){
    logger |> writeLog(type = "error", "shape must be an sf object")
    return()
  }
  
  if (!inherits(shape, "sf")){
    return(async |> asyncLog(type = "error", "shape must be an sf object"))
  }
```

- Document the function using `{roxygen2}` tags - see other functions in the `R/` directory for examples.
- Open the `test-fit_improved.R` file in the `tests/testthat/` directory and add tests that check the error handling checks function correctly and that the function produces the desired results. 
- You can find various example data objects in `tests/testthat/helper_data.R` that may help to test the function. 
- Document the package using `devtools::document()` and then install with `devtools::install()`
- Open the test file and click the "Run Tests" in the top right of the window pane.
- Once the tests are all passing you can begin to develop the Shiny module.

### Shiny module

- The shiny module consists of four files in the `inst/shiny/modules/` directory named with the module identifier and ending in `.yml`, `.R`, `.md` and `.Rmd`

#### Configuration

- The `.yml` file is a configuration file and contains five lines:
  - `component` contains the component the module belongs to e.g. `"fit"` 
  - `short_name` contains a short name for the module and is used in the user interface so should be formatted accordingly e.g. `"Fit improved model`"
  - `long_name` contains a longer name for the module and is also used in the user interface e.g. `"Fit an improved model using the new method"`
  - `authors` contains your name: e.g. `"Simon E H Smart"`
  - `package` contains a list of packages used in the module inside square brackets and without quotes e.g. `[terra, disaggregation]`
  - Add an extra line at the end of the file called `class: ` and then add `mandatory`, `choice` or `optional`  after it (without quotes) depending on whether it is mandatory to use the module, it is one of several of choices but one must be used or it is optional.
  Depending on the function of the module, you may need to update the `.yml` of other modules in the component, for example currently the `fit_fit` module is `mandatory` but if we were to add a new module to the component 
  that could be changed to `choice`.
  
#### UI and server

- Data is passed between modules and loaded after being saved through the `common` object, defined in  the `common.R` file in the `inst/shiny/` directory. If the module uses data created by previous modules you need to find 
the name of these objects in that file, for example the response data is stored in `common$shape` and the unprepared covariates in `common$covs`. If the module produces a new type of object then you need to add a new object to the 
`common` data structure by editing the `common.R` file. This is not necessary if, for example the module adds a new method for uploading response data or another covariate as these would add their results to `common$shape` 
and `common$covs` respectively.
- The .`R` file is the main module file and contains several sections:
  - The `<identifier>_module_ui` function contains the \*Input elements which should correspond with the parameters of the function. See https://shiny.posit.co/r/components/ for available input widgets you can use. 
  The input ids need wrapping inside `ns()` to create ids that are unique to the module. The template contains an `actionButton()` which when clicked runs the code inside the `*_module_server` function. 
  - The `<identifier>_module_server` function contains an `observeEvent()` which is run when the `actionButton()` in the UI is clicked. 
  - The structure of the code inside the module server differs depending on whether the module runs synchronously or asynchronously. For synchronous modules:
    - In the *warning* block, examine inputs to check that they are as expected and issue warnings if not using `common$logger |> writeLog()`. These checks are distinct from those inside the function as they
    are checking that the state of the app is correct - for example if the module requires the data to have been prepared, you should check that `common$prep` is not `NULL`. See the documentation of `shinyscholar::writeLog()` for more details.
    - In the *function call* block, pass the inputs to the module function.
    - In the *load into common* block, store the result(s) of the function call in the relevant `common` object.
    - The *metadata* block can be left blank for now.
    - In the *trigger* block, `trigger()` is called which can be used to trigger actions elsewhere in the module or app using `watch()`. This should not need editing.
    - In the *result* block, use the relevant `common` objects to produce outputs like plots using `render*` objects which will be passed to the `_module_result` function. Inside each `render*` function you should 
    add a `req()` containing the common object created by the module to stop it executing before the object exists and a `watch()` to trigger the execution once the object is created.
    - The *return* block, block can be left blank for now.
  - For asynchronous modules, the principles are the same but the button only triggers the function to start and a separate `results` section waits for the results and processes them. Much of the setup is handled automatically in the template module.
    - Add checks in the *warning* block as before
    - Rather than passing the inputs to the function directly, pass them instead to for example `common$tasks$fit_improved$invoke()`
    - Wrap any `SpatRaster` objects are present in the function inputs using `terra::wrap()`
    - Inside the results section, you should check the class of the object returned by the function and only load the result into `common` if it is the correct type. If an error is returned it will be sent to the logger.
    - Unwrap any `SpatRaster` objects in the returned object using `terra::unwrap()`
  - The `<identifier>_module_result` function contains the `*Output()` functions that render the results created in the *result* block of the module server. As in the `*_module_ui` function, the object ids need wrapping inside `ns()`.
  - The `<identifier>_module_map` function updates the `{leaflet}` map. `map` is a `leafletProxy()` object created in the main server function so leaflet functions can be piped to it e.g. `map |> addRasterImage()`. There are
  several functions in the package that facilitate adding objects to the map e.g. `raster_map()` and `shape_map()` which you can either use directly or modify to suit your needs. If you are unfamiliar with leaflet, it is 
  probably sensible to develop this function outside of Shiny. 
  - The `*_module_rmd` function can be left blank for now.

#### Guidance
  
- The `.md` file contains the module guidance. 
- The background section should explain what the module does and what the scientific basis for it is. 
- The implementation section should explain how the module works (e.g. which functions it uses) and how the user can use it i.e. what do they need to click.
- Add any relevant references in the references section.

#### Reproducibility

- The `.Rmd` file contains the Rmarkdown required to reproduce the module. Leave this blank for now.

### Changes to other files

- If you have added a `resp`, `cov` or `agg` module, some other modules need updating to listen for changes made by the module by adding e.g. `watch("fit_improved")`. Modules to update are as follows:
  - For `resp` modules update `resp_simplify.R`, `prep_mesh.R` and `prep_final.R`. You should also add `watch("resp_edit")` to your own module underneath the existing `watch()` line.
  - For `cov` modules update `prep_summary.R` and `rep_covs.R`.
  - For `agg` modules update `prep_summary.R` and `rep_covs.R`. 
- Open the `global.R` file in the `inst/shiny/` directory and navigate to the line that creates `base_module_configs`. Add a new line to the object in the relevant position to load the new module. 
The order of modules is important as this object defines the order of chunks in the Rmarkdown and so modules must be placed after any modules that they use the results of. 
For example if our `fit_improved` module used the results of the `fit_fit` module, it needs to be placed after it in the list.
- If your module created new objects in `common` that contain a `SpatRaster`, the `core_save.R` and `core_load.R` files need to be edited to wrap and unwrap them. Use `wrap_terra()` and `unwrap_terra()` for this as they
can handle cases where the objects are `NULL` e.g. when your module has not been used. Objects need wrapping before saving, unwrapping after saving and unwrapping after loading. There are many other lines already doing this for other objects.

### Manual checks

- Now you can run the app and check your module works. It may be helpful to create a save file of the app at a stage immediately prior to running your module so that you can reload it quickly if the app crashes. 
If you create a variable called `load_file_path` that contains the path to the load file, it will be loaded automatically when the app starts.

### Reproducibility

- Once the module is running correctly run `shinyscholar::save_and_load(".", "fit_improved")` to add lines of code to the module that save and load the inputs and then `shinyscholar::metadata(".", "fit_improved")` to add 
lines to the *metadata* block in the module server, the module_rmd function and to the `.Rmd` file.
- Edit the `.Rmd` to use the input variables to call the module function. The variables are added automatically by `shinyscholar::metadata()` and take the form of e.g. `{{fit_improved_input1}}`. The curly brackets are 
required for the input values to be transferred when the user reproduces their analysis. The `common` objects have corresponding variables inside the `.Rmd` files of the modules and these need to used so that the chunk
of the new module can be linked to previous chunks. For example `common$prep` containing the prepared data is replaced by `prepared_data` in the `.Rmd` files.

### End-to-end tests

- Create an end-to-end test using `{shinytest2}`. You are probably best of using an existing test as a template. 
- They consist of two files - a helper file e.g. `helper-fit_improved.R` which runs the app and saves it after running the module and the module test file which you created earlier to test the module's function. 
This architecture is necessary because saving the app in tests fails erratically and this setup allows the test to be rerun until it saves successfully. 
- In the `helper-fit_improved.R` you start the app, set any necessary input values and then run the module. Add a section to `test-fit_improved.R` that calls then function defined 
in the helper function using e.g. `rerun_test_setup("fit_improved_test")` and which then reads the saved file and checks that the `common` object modified or created in the module is in the correct state. Note that if the 
module runs asynchronously you need to wait for it to complete by adding `app$wait_for_value(input = "fit_improved-complete")` before saving the app.
- Run all the tests using `devtools::test()`.

### Finally
- Commit your changes, push to Github and create a pull request to add the module.

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
