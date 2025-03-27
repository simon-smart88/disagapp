cov_landuse_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    selectInput(ns("uses"), "Land uses", multiple = TRUE,
                       choices = c("Bare", "BuiltUp", "Crops", "Grass",
                                    "MossLichen", "PermanentWater", "SeasonalWater",
                                  "Shrub", "Snow", "Tree")),
    selectInput(ns("year"), "Year", choices = 2015:2019, selected = 2019),
    bslib::input_task_button(ns("run"), "Download data")
  )
}

cov_landuse_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    common$tasks$cov_landuse <- ExtendedTask$new(function(...) {
      promises::future_promise({
        cov_landuse(...)
      })
    }) |> bslib::bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####
    if (curl::has_internet() == FALSE){
      common$logger |> writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (is.null(input$uses)) {
      common$logger |> writeLog(type = "error", "Please select the land use categories to download")
      return()
    }

    if (is.null(common$shape)) {
      common$logger |> writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    common$tasks$cov_landuse$invoke(common$shape, input$year, input$uses, TRUE)
    common$logger |> writeLog(type = "starting", "Starting to download land use data")
    results$resume()
    # METADATA ####
    common$meta$cov_landuse$used <- TRUE
    common$meta$cov_landuse$uses <- input$uses
    common$meta$cov_landuse$year <- input$year
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$cov_landuse$result()
    results$suspend()
    if (inherits(result, "list")){
      result <- unwrap_terra(result)
      common$covs <- append(common$covs, result)
      common$logger |> writeLog(type = "complete", "Land use data has been downloaded")
      # TRIGGER
      trigger("cov_landuse")
      do.call("cov_landuse_module_map", list(map, common))
      shinyjs::runjs("Shiny.setInputValue('cov_landuse-complete', 'complete');")
    } else {
      common$logger |> writeLog(type = "error", result)
    }
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      uses = input$uses,
      year = input$year)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "uses", selected = state$uses)
      updateSelectInput(session, "year", selected = state$year)
    }
  ))
})
}

cov_landuse_module_map <- function(map, common) {
  for (use in common$meta$cov_landuse$uses){
    land_use <- paste0(use, " land use")
    raster_map(map, common, common$covs[[land_use]], land_use)
  }
}

cov_landuse_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_landuse_knit = !is.null(common$meta$cov_landuse$used),
    cov_landuse_uses = printVecAsis(common$meta$cov_landuse$uses),
    cov_landuse_year = common$meta$cov_landuse$year
  )
}

