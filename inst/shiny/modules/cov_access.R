cov_access_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    selectInput(ns("layer"), "Layer", choices = c("Travel Time to Cities (2015)",
                                                  "Motorized Travel Time to Healthcare (2020)",
                                                  "Walking Only Travel Time to Healthcare (2020)")),
    actionButton(ns("run"), "Download data")
  )
}

cov_access_module_server <- function(id, common) {
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
    if (is.null(common$covs)){
      common$covs <- list(access)
    } else {
      common$covs <- append(common$covs, access)
    }
    close_loading_modal()
    # METADATA ####
    common$meta$access$used <- TRUE
    common$meta$access$layer <- input$layer
    # TRIGGER
    gargoyle::trigger("cov_access")
  })

  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}

cov_access_module_map <- function(map, common) {
  # Map logic
}

cov_access_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_access_knit = common$meta$access$used
  )
}

