core_save_module_ui <- function(id) {
  ns <- NS(id)
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
        show_loading_modal("Please wait while the session is saved")



        temp <- list()
        common_items <- names(common)
        # exclude the non-public and function objects
        save_items  <- common_items[!common_items %in% c("clone", ".__enclos_env__", "logger", "reset", "tasks", "add_map_layer", "map_layers")]
        temp[save_items] <- as.list(common)[save_items]
        # save logger minus the header
        temp$logger <- strsplit(common$logger(), "-----<br>")[[1]][3]
        class(temp) <- "common"

        temp$state$main <- list(
          selected_module = sapply(COMPONENTS, function(x) main_input[[glue("{x}Sel")]], simplify = FALSE)
        )

        temp$state$main$version <- as.character(packageVersion("disagapp"))
        temp$state$main$app <- "disagapp"

        # Save module state
        for (module_id in names(modules)) {
          temp$state[[module_id]] <- modules[[module_id]]$save()
        }

        # wrap terra objects prior to save
        temp$covs <- wrap_terra(temp$covs)
        temp$covs_prep <- wrap_terra(temp$covs_prep)
        temp$covs_prep_lores <- wrap_terra(temp$covs_prep_lores)
        temp$agg <- wrap_terra(temp$agg)
        temp$agg_prep <- wrap_terra(temp$agg_prep)
        temp$agg_prep_lores <- wrap_terra(temp$agg_prep_lores)

        temp$pred$`prediction (rate)` <- wrap_terra(temp$pred$`prediction (rate)`)
        temp$pred$`prediction (cases)` <- wrap_terra(temp$pred$`prediction (cases)`)
        temp$pred$covariates <- wrap_terra(temp$pred$covariates)
        temp$pred$iid <- wrap_terra(temp$pred$iid)
        temp$pred$field <- wrap_terra(temp$pred$field)
        temp$pred$uncertainty_lower <- wrap_terra(temp$pred$uncertainty_lower)
        temp$pred$uncertainty_upper <- wrap_terra(temp$pred$uncertainty_upper)
        temp$transfer$agg <- wrap_terra(temp$transfer$agg)
        temp$transfer$cases <- wrap_terra(temp$transfer$cases)
        temp$transfer$prediction <- wrap_terra(temp$transfer$prediction)
        temp$transfer$field <- wrap_terra(temp$transfer$field)
        temp$transfer$covariates <- wrap_terra(temp$transfer$covariates)

        # remove large duplicate objects
        temp$prep$covariate_rasters <- NULL
        temp$fit$data$covariate_rasters <- NULL
        temp$fit$data$covariate_data <- NULL
        temp$fit$data$polygon_shapefile <- NULL
        temp$fit$data$polygon_data <- NULL
        temp$fit$data$aggregation_pixels <- NULL
        temp$fit$data$coords_for_fit <- NULL
        temp$fit$data$coords_for_prediction <- NULL

        on.exit(close_loading_modal())
        on.exit(rm("temp"), add = TRUE)

        # save file
        saveRDS(temp, file)

      }
    )
  })
}
