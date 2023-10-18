cov_access_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Download data")
  )
}

cov_access_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload incidence data first")
      return()
    }
    # FUNCTION CALL ####
    showModal(modalDialog(title = "Info", "Please wait while the data is loaded.
                          This window will close once it is complete.", easyClose = FALSE))
    access <- cov_access(common$shape)
    # LOAD INTO COMMON ####
    common$covs <- append(common$covs, access)
    removeModal()
    # METADATA ####
    common$meta$access$used <- TRUE
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

