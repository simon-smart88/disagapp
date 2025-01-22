core_load_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
  h4("Load session"),
  includeMarkdown("Rmd/text_loadsesh.Rmd"),
  fileInput(ns("file"), "", accept = ".rds"),
  actionButton(ns("run"), "Load RDS")
  )
  }

core_load_module_server <- function(id, common, modules, map, COMPONENT_MODULES, parent_session) {
  moduleServer(id, function(input, output, session) {

    observe({
      shinyjs::toggleState("run", !is.null(input$file$datapath))
    })

    load_session <- function(path){
      temp <- readRDS(path)

      if (!("common" %in% class(temp))){
        close_loading_modal()
        common$logger |> writeLog(type = "error", "That is not a valid Disagapp save file")
        return()
      }

      if (temp$state$main$version != as.character(packageVersion("disagapp"))){
        new_version <- as.character(packageVersion("disagapp"))
        common$logger |> writeLog(type = "warning",
                                  glue::glue("The save file was created using Disagapp v{temp$state$main$version}, but you are using Disagapp v{new_version}"))
      }

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
      common$fit$data$covariate_rasters <- unwrap_terra(common$fit$data$covariate_rasters)
      common$pred$`prediction (rate)` <- unwrap_terra(common$pred$`prediction (rate)`)
      common$pred$`prediction (cases)` <- unwrap_terra(common$pred$`prediction (cases)`)
      common$pred$covariates <- unwrap_terra(common$pred$covariates)
      common$pred$iid <- unwrap_terra(common$pred$iid)
      common$pred$field <- unwrap_terra(common$pred$field)
      common$pred$uncertainty_lower <- unwrap_terra(common$pred$uncertainty_lower)
      common$pred$uncertainty_upper <- unwrap_terra(common$pred$uncertainty_upper)
      common$transfer$agg <- unwrap_terra(common$transfer$agg)
      common$transfer$cases <- unwrap_terra(common$transfer$cases)
      common$transfer$prediction <- unwrap_terra(common$transfer$prediction)
      common$transfer$field <- unwrap_terra(common$transfer$field)
      common$transfer$covariates <- unwrap_terra(common$transfer$covariates)

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
    }

    observeEvent(input$run, {
      show_loading_modal("Please wait while the session is restored")
      load_session(input$file$datapath)
      close_loading_modal()
      if (isFALSE(getOption("shiny.testmode"))) {
        common$logger |> writeLog(type = "info", "The previous session has been loaded successfully")
      }
    })

    # load file if run_disagapp has a load_file parameter
    load_file_path <- reactive({if (exists("load_file_path", envir = .GlobalEnv)) {
      get("load_file_path", envir = .GlobalEnv)
    } else {
      NULL
    }})

    load_on_start <- observe({
      req(load_file_path())
      show_loading_modal("Loading previous session")
      load_session(load_file_path())
      close_loading_modal()
      common$logger %>% writeLog(type="info", "The previous session has been loaded successfully")
      load_on_start$destroy()
    })

  }
)}

