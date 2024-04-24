core_save_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    br(),
    h5(em("Note: To save your session code or metadata, use the Reproduce component")),
    wellPanel(
      h4(strong("Save Session")),
      p(paste0("By saving your session into an RDS file, you can resume ",
               "working on it at a later time or you can share the file",
               " with a collaborator.")),
      shinyjs::hidden(p(
        id = "save_warning",
        icon("triangle-exclamation"),
        paste0("The current session data is large, which means the ",
               "downloaded file may be large and the download might",
               " take a long time.")
      )),
      downloadButton(ns("save_session"), "Save Session"),
      br()
    )
  )
}

core_save_module_server <- function(id, common, modules, COMPONENTS, main_input) {
  moduleServer(id, function(input, output, session) {

    #this is currently a bit pointless as unpacked terra objects won't be included...
    observe({
      common_size <- as.numeric(utils::object.size(common))
      shinyjs::toggle("save_warning", condition = (common_size >= 100 * MB))
    })

    output$save_session <- downloadHandler(
      filename = function() {
        paste0("disagapp-session-", Sys.Date(), ".rds")
      },
      content = function(file) {
        show_loading_modal("Please wait the session is saved")

        common$state$main <- list(
          selected_module = sapply(COMPONENTS, function(x) main_input[[glue("{x}Sel")]], simplify = FALSE)
        )

        # Ask each module to save whatever data it wants
        for (module_id in names(modules)) {
          common$state[[module_id]] <- modules[[module_id]]$save()
        }

        # wrap terra objects prior to save
        common$covs <- wrap_terra(common$covs)
        common$covs_prep <- wrap_terra(common$covs_prep)
        common$covs_prep_lores <- wrap_terra(common$covs_prep_lores)
        common$agg <- wrap_terra(common$agg)
        common$agg_prep <- wrap_terra(common$agg_prep)
        common$agg_prep_lores <- wrap_terra(common$agg_prep_lores)
        common$prep$covariate_rasters <- wrap_terra(common$prep$covariate_rasters)
        common$pred$field <- wrap_terra(common$pred$field)
        common$pred$prediction <- wrap_terra(common$pred$prediction)
        common$pred$covariates <- wrap_terra(common$pred$covariates)
        common$fit$data$covariate_rasters <- wrap_terra(common$fit$data$covariate_rasters)
        common$transfer$agg <- wrap_terra(common$transfer$agg)
        common$transfer$cases <- wrap_terra(common$transfer$cases)
        common$transfer$prediction <- wrap_terra(common$transfer$prediction)
        common$transfer$field <- wrap_terra(common$transfer$field)
        common$transfer$covariates <- wrap_terra(common$transfer$covariates)

        #save file
        saveRDS(common, file)

        #unwrap the terra objects
        common$covs <- unwrap_terra(common$covs)
        common$covs_prep <- unwrap_terra(common$covs_prep)
        common$covs_prep_lores <- unwrap_terra(common$covs_prep_lores)
        common$agg <- unwrap_terra(common$agg)
        common$agg_prep <- unwrap_terra(common$agg_prep)
        common$agg_prep_lores <- unwrap_terra(common$agg_prep_lores)
        common$prep$covariate_rasters <- unwrap_terra(common$prep$covariate_rasters)
        common$pred$field <- unwrap_terra(common$pred$field)
        common$pred$prediction <- unwrap_terra(common$pred$prediction)
        common$pred$covariates <- unwrap_terra(common$pred$covariates)
        common$fit$data$covariate_rasters <- unwrap_terra(common$fit$data$covariate_rasters)
        common$transfer$agg <- unwrap_terra(common$transfer$agg)
        common$transfer$cases <- unwrap_terra(common$transfer$cases)
        common$transfer$prediction <- unwrap_terra(common$transfer$prediction)
        common$transfer$field <- unwrap_terra(common$transfer$field)
        common$transfer$covariates <- unwrap_terra(common$transfer$covariates)

        close_loading_modal()
      }
    )
  })
}
