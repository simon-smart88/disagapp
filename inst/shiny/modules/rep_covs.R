rep_covs_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    downloadButton(ns("download"), "Download covariates")
  )
}

rep_covs_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

    output$download <- downloadHandler(
      filename = function() {
        paste0("disagapp-covariates-", Sys.Date(), ".zip")
      },
      content = function(file) {

        common$meta$rep_covs$used <- TRUE

        directory <- tempdir()
        dir.create(file.path(directory, "rep_cov"))

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

        gargoyle::trigger("rep_covs")

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
