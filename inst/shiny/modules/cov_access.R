cov_access_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("layer"), "Layer", choices = c("Travel Time to Cities (2015)",
                                                  "Motorized Travel Time to Healthcare (2020)",
                                                  "Walking Only Travel Time to Healthcare (2020)")),
    bslib::input_task_button(ns("run"), "Download data")
  )
}

cov_access_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  common$tasks$cov_access <- ExtendedTask$new(function(...) {
    promises::future_promise({
      cov_access(...)
    }, seed = TRUE)
  }) |> bslib::bind_task_button("run")

  observeEvent(input$run, {
    # WARNING ####
    if (curl::has_internet() == FALSE){
      common$logger |> writeLog(type = "error", "This module requires an internet connection")
      return()
    }

    if (is.null(common$shape)) {
      common$logger |> writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    common$tasks$cov_access$invoke(common$shape, input$layer, TRUE)
    common$logger |> writeLog(type = "starting", "Starting to download accessibility data")
    results$resume()
    # METADATA ####
    common$meta$cov_access$used <- TRUE
    common$meta$cov_access$layer <- input$layer

  })

    results <- observe({
      # LOAD INTO COMMON ####
      result <- common$tasks$cov_access$result()
      results$suspend()
      if (inherits(result, "PackedSpatRaster")){
        common$covs[[common$meta$cov_access$layer]] <- unwrap_terra(result)
        common$logger |> writeLog(type = "complete", "Accessibility data has been downloaded")
        # TRIGGER
        gargoyle::trigger("cov_access")
        do.call("cov_access_module_map", list(map, common))
        shinyjs::runjs("Shiny.setInputValue('cov_access-complete', 'complete');")
      } else {
        common$logger |> writeLog(type = "error", result)
      }
  })

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      layer = input$layer)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateSelectInput(session, "layer", selected = state$layer)
    }
  ))
})
}

cov_access_module_map <- function(map, common) {
  layer <- common$meta$cov_access$layer
  raster_map(map, common, common$covs[[layer]], layer)
}

cov_access_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_access_knit = !is.null(common$meta$cov_access$used),
    access_layer = common$meta$cov_access$layer
  )
}

