rep_covs_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    downloadButton(ns("dl"), "Download covariates")
  )
}

rep_covs_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

    output$save_session <- downloadHandler(
      filename = function() {
        paste0("disagapp-covariates-", Sys.Date(), ".zip")
      },
      content = function(file) {
        show_loading_modal("Please wait the session is saved")


        # wrap terra objects prior to save
        common$covs <- wrap_terra(common$covs)
        common$covs_prep <- wrap_terra(common$covs_prep)
        common$covs_prep_lores <- wrap_terra(common$covs_prep_lores)
        common$agg <- wrap_terra(common$agg)
        common$agg_prep <- wrap_terra(common$agg_prep)
        common$agg_prep_lores <- wrap_terra(common$agg_prep_lores)
        common$prep$covariate_rasters <- wrap_terra(common$prep$covariate_rasters)
      }
    )

})
}


rep_covs_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    rep_covs_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

