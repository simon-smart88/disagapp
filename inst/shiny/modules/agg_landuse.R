agg_landuse_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    selectInput(ns("uses"), "Land uses", multiple = FALSE,
                choices = c("Bare", "BuiltUp", "Crops", "Grass",
                            "Shrub", "Tree")),
    selectInput(ns("year"), "Year", choices = 2015:2019, selected = 2019),
    bslib::input_task_button(ns("run"), "Download data")
  )
}

agg_landuse_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  common$tasks$agg_landuse <- ExtendedTask$new(function(...) {
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
    common$tasks$agg_landuse$invoke(common$shape, input$year, input$uses, TRUE)
    common$logger |> writeLog(type = "starting", "Starting to download land use data")
    results$resume()
    # METADATA ####
    common$meta$agg_landuse$used <- TRUE
    common$meta$agg_landuse$uses <- input$uses
    common$meta$agg_landuse$year <- input$year
    common$meta$agg_landuse$log <- FALSE
  })

  results <- observe({
    # LOAD INTO COMMON ####
    result <- common$tasks$agg_landuse$result()
    results$suspend()
    if (inherits(result, "list")){
      result <- unwrap_terra(result)
      common$agg <- result[[1]]
      names(common$agg) <- paste0(common$meta$agg_landuse$uses, " land use aggregation")
      common$logger |>  writeLog(type = "complete", "Land use data has been downloaded")
      # TRIGGER
      trigger("agg_landuse")
      do.call("agg_landuse_module_map", list(map, common))
      shinyjs::runjs("Shiny.setInputValue('agg_landuse-complete', 'complete');")
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

agg_landuse_module_map <- function(map, common) {
  raster_map(map, common, common$agg, names(common$agg))
}

agg_landuse_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    agg_landuse_knit = !is.null(common$meta$agg_landuse$used),
    agg_landuse_uses = printVecAsis(common$meta$agg_landuse$uses),
    agg_landuse_year = common$meta$agg_landuse$year
  )
}

