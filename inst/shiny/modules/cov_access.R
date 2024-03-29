cov_access_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("layer"), "Layer", choices = c("Travel Time to Cities (2015)",
                                                  "Motorized Travel Time to Healthcare (2020)",
                                                  "Walking Only Travel Time to Healthcare (2020)")),
    actionButton(ns("run"), "Download data")
  )
}

cov_access_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    access <- cov_access(common$shape, input$layer)
    # LOAD INTO COMMON ####
    common$covs[[input$layer]] <- access
    common$logger %>% writeLog("Accessibility data has been downloaded")
    close_loading_modal()
    # METADATA ####
    common$meta$cov_access$used <- TRUE
    common$meta$cov_access$layer <- input$layer
    # TRIGGER
    gargoyle::trigger("cov_access")
  })

  return(list(
    save = function() {
list(layer = input$layer)
    },
    load = function(state) {
updateSelectInput(session, "layer", selected = state$layer)
    }
  ))
})
}

cov_access_module_map <- function(map, common) {
  layer <- common$meta$cov_access$layer
  covariate_map(map, common, common$covs[[layer]], layer)
}

cov_access_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_access_knit = !is.null(common$meta$cov_access$used),
    access_layer = common$meta$cov_access$layer
  )
}

