prep_scale_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    actionButton(ns("run"), "Scale covariates")
  )
}

prep_scale_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$covs_prep)) {
      common$logger |> writeLog(type = "error", "Please resample the covariates first")
      return()
    }
    # FUNCTION CALL ####
    scaled_covariates <- prep_scale(common$covs_prep)
    common$logger |> writeLog(type = "complete", "Covariates have been scaled")

    # LOAD INTO COMMON ####
    common$covs_prep <- scaled_covariates[["covariates"]]

    # METADATA ####
    common$meta$prep_scale$used <- TRUE
    common$meta$prep_scale$parameters <- scaled_covariates[["parameters"]]

    # TRIGGER
    gargoyle::trigger("prep_scale")
    do.call("prep_scale_module_map", list(map, common))
  })

})
}

prep_scale_module_map <- function(map, common){
  for (layer in names(common$covs_prep)){
    raster_map(map, common, common$covs_prep[[layer]], layer)
  }
}

prep_scale_module_rmd <- function(common) {
  list(
    prep_scale_knit = !is.null(common$meta$prep_scale$used)
  )
}

