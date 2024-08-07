core_load_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
  h4("Load session"),
  includeMarkdown("Rmd/text_loadsesh.Rmd"),
  fileInput(ns("load_session"), "", accept = ".rds"),
  actionButton(ns("goLoad_session"), "Load RDS")
  )
  }

core_load_module_server <- function(id, common, modules, map, COMPONENT_MODULES, parent_session) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggleState("goLoad_session", !is.null(input$load_session$datapath))
    })

    observeEvent(input$goLoad_session, {
      show_loading_modal("Please wait while the session is restored")
      temp <- temp <- readRDS(input$load_session$datapath)
      temp_names <- names(temp)
      #exclude the non-public and function objects
      temp_names  <- temp_names[!temp_names %in% c("clone", ".__enclos_env__", "add_map_layer", "logger", "map_layers", "reset", "tasks")]
      for (name in temp_names){
        common[[name]] <- temp[[name]]
      }

      #blank map
      gargoyle::trigger("clear_map")
      common$map_layers = NULL

      # Ask each module to load its own data
      for (module_id in names(common$state)) {
        if (module_id != "main"){
          modules[[module_id]]$load(common$state[[module_id]])
        }}

      for (component in names(common$state$main$selected_module)) {
        value <- common$state$main$selected_module[[component]]
        updateRadioButtons(session, glue("{component}Sel"), selected = value)
      }

      #required due to terra objects being pointers to c++ objects
      #unwrap the terra objects
      common$covs <- unwrap_terra(common$covs)
      common$covs_prep <- unwrap_terra(common$covs_prep)
      common$covs_prep_lores <- unwrap_terra(common$covs_prep_lores)
      common$agg <- unwrap_terra(common$agg)
      common$agg_prep <- unwrap_terra(common$agg_prep)
      common$agg_prep_lores <- unwrap_terra(common$agg_prep_lores)
      common$prep$covariate_rasters <- unwrap_terra(common$prep$covariate_rasters)
      common$pred$field <- unwrap_terra(common$pred$field)
      common$pred$`prediction (rate)` <- unwrap_terra(common$pred$`prediction (rate)`)
      common$pred$`prediction (cases)` <- unwrap_terra(common$pred$`prediction (cases)`)
      common$pred$iid <- unwrap_terra(common$pred$iid)
      common$fit$data$covariate_rasters <- unwrap_terra(common$fit$data$covariate_rasters)
      common$transfer$agg <- unwrap_terra(common$transfer$agg)
      common$transfer$cases <- unwrap_terra(common$transfer$cases)
      common$transfer$prediction <- unwrap_terra(common$transfer$prediction)
      common$transfer$field <- unwrap_terra(common$transfer$field)
      common$transfer$covariates <- unwrap_terra(common$transfer$covariates)
      common$pred$uncertainty$predictions_ci$`lower CI` <- unwrap_terra(common$pred$uncertainty$predictions_ci$`lower CI`)
      common$pred$uncertainty$predictions_ci$`upper CI` <- unwrap_terra(common$pred$uncertainty$predictions_ci$`upper CI`)

      #restore map and results for used modules
      for (used_module in names(common$meta)){
        gargoyle::trigger(used_module) # to replot results
      }

      if (!("prep_summary" %in% names(common$meta))){
        for (used_module in names(common$meta)){
          component <- strsplit(used_module, "_")[[1]][1]
          map_fx <- COMPONENT_MODULES[[component]][[used_module]]$map_function
          if (!is.null(map_fx)) {
            do.call(map_fx, list(map, common = common))
          }
        }
      } else {
        #find the last prep_ module
        prep <- names(common$meta)[grep("prep_", names(common$meta))]
        prep <- prep[prep != "prep_mesh"]
        prep <- prep[length(prep)]
        used_modules <- c("prep_mesh", prep, names(common$meta)[grep("pred_", names(common$meta))])
        shape_map(map, common)
        for (used_module in names(common$meta)){
          component <- strsplit(used_module, "_")[[1]][1]
          map_fx <- COMPONENT_MODULES[[component]][[used_module]]$map_function
          if (!is.null(map_fx)) {
            do.call(map_fx, list(map, common = common))
          }
        }
      }

      close_loading_modal()
      common$logger |> writeLog(type = "info", "The previous session has been loaded successfully")
    })

  }
)}

