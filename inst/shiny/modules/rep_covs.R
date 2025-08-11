rep_covs_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dlbutton"))

  )
}

rep_covs_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    output$dlbutton <- renderUI({
      watch("cov_access")
      watch("cov_bioclim")
      watch("cov_landuse")
      watch("cov_nightlight")
      watch("cov_water")
      watch("cov_upload")
      if (length(common$covs) > 0 ){
        shinyjs::runjs("Shiny.setInputValue('rep_covs-ready', 'complete');")
        downloadButton(session$ns("download"), "Download covariates")
      } else {
        actionButton(session$ns("dummy"), "Download covariates", icon = icon("download"))
      }
    })

    observeEvent(input$dummy, {
      common$logger |> writeLog(type = "error",
                                "Please load some covariates first")
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0("disagapp-covariates-", Sys.Date(), ".zip")
      },
      content = function(file) {

        common$meta$rep_covs$used <- TRUE

        directory <- tempfile()
        dir.create(file.path(directory, "rep_cov"), recursive = TRUE)

        if (is.null(common$prep)){
          common$meta$rep_covs$pre_prep <- TRUE
          lapply(names(common$covs), function(x)
            terra::writeRaster(common$covs[[x]], overwrite = TRUE,
                               filename = file.path(directory, "rep_cov", paste0(x, ".tif"))))
            if (!is.null(common$agg)){
              terra::writeRaster(common$agg, overwrite = TRUE,
                                 filename = file.path(directory, "rep_cov", "aggregation.tif"))
            }
        }

        if (!is.null(common$covs_prep)) {
          common$meta$rep_covs$post_prep <- TRUE
          terra::writeRaster(common$covs_prep, overwrite = TRUE,
                             filename = file.path(directory, "rep_cov", "covariates.tif"))
          terra::writeRaster(common$agg_prep, overwrite = TRUE,
                             filename = file.path(directory, "rep_cov", "aggregation.tif"))
        }

        if (!is.null(common$prep) & (!is.null(common$meta$prep_resolution$used))){
          common$meta$rep_covs$low_res <- TRUE
          terra::writeRaster(common$covs_prep_lores, overwrite = TRUE,
                             filename = file.path(directory, "rep_cov", "covariates_low_resolution.tif"))
          terra::writeRaster(common$agg_prep_lores, overwrite = TRUE,
                             filename = file.path(directory, "rep_cov", "aggregation_low_resolution.tif"))
        }

        owd <- setwd(file.path(directory, "rep_cov"))
        on.exit(setwd(owd))

        files <- list.files(".")

        trigger("rep_covs")

        zip::zipr(zipfile = file,
                  files = files,
                  mode = "mirror",
                  include_directories = FALSE)

      }
    )

})
}


rep_covs_module_rmd <- function(common) {
  list(
    rep_covs_knit = !is.null(common$meta$rep_covs$used),
    knit_pre_prep = !is.null(common$meta$rep_covs$pre_prep),
    knit_post_prep = !is.null(common$meta$rep_covs$post_prep),
    knit_low_res = !is.null(common$meta$rep_covs$low_res)
  )
}
