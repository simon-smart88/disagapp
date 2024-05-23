function(input, output, session) {

  future::plan(future::multisession)

  ########################## #
  # LOAD COMMON ####
  ########################## #
  source(system.file("shiny/common.R", package = "disagapp"))
  common <- common_class$new()

  ########################## #
  # LOGGER ####
  ########################## #

  # Variable to keep track of current log message
  initLogMsg <- function() {
    intro <- "***WELCOME TO DISAGAPP***"
    brk <- paste(rep("------", 14), collapse = "")
    expl <- "Please find messages for the user in this log window."
    logInit <- gsub(".{4}$", "", paste(intro, brk, expl, brk, "", sep = "<br>"))
    logInit
  }

  common$logger <- reactiveVal(initLogMsg())

  # Write out logs to the log Window
  observeEvent(common$logger(), {
    shinyjs::html(id = "logHeader", html = common$logger(), add = FALSE)
    shinyjs::js$scrollLogger()
  })

  check_timer <- reactiveTimer(5000)
  check_online <- observe({
    check_timer()
    if (curl::has_internet() == FALSE){
      shinyalert::shinyalert("An internet connection is required to use some functions of disagapp. Please connect in order to make full use of the app.",
                             type = "warning")
      check_online$destroy()
    }
  })

  output$running_tasks <- renderText({
    status <- unlist(lapply(common$tasks, function(x){x$status()}))
    running <- length(status[status == "running"])
    glue::glue("There are currently {running} tasks running")
  })

  ########################## #
  # REACTIVE VALUES LISTS ####
  ########################## #

  # tab and module-level reactives
  component <- reactive({
    input$tabs
  })
  observe({
    if (component() == "_stopapp") {
      shinyjs::runjs("window.close();")
      stopApp()
    }
  })
  module <- reactive({
    if (component() == "intro") "intro"
    else input[[glue("{component()}Sel")]]
  })

  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #

  # UI for component guidance text
  output$gtext_component <- renderUI({
    file <- file.path("Rmd", glue("gtext_{component()}.Rmd"))
    if (!file.exists(file)) return()
    includeMarkdown(file)
  })

  # UI for module guidance text
  output$gtext_module <- renderUI({
    req(module())
    file <- COMPONENT_MODULES[[component()]][[module()]]$instructions
    if (is.null(file)) return()
    includeMarkdown(file)
  })

  # Help Component
  help_components <- COMPONENTS[!COMPONENTS == "rep"]
  lapply(help_components, function(component) {
    btn_id <- paste0(component, "Help")
    observeEvent(input[[btn_id]], updateTabsetPanel(session, "main", "Component Guidance"))
  })

  # Help Module
  lapply(help_components, function(component) {
    lapply(COMPONENT_MODULES[[component]], function(module) {
      btn_id <- paste0(module$id, "Help")
      observeEvent(input[[btn_id]], updateTabsetPanel(session, "main", "Module Guidance"))
      })})

  ####################
  ### INITIALISATION ####
  ###################

  gargoyle::init("intro")
  # Initialize all modules
  modules <- list()
  lapply(names(COMPONENT_MODULES), function(component) {
    lapply(COMPONENT_MODULES[[component]], function(module) {
      # Initialize event triggers for each module
      gargoyle::init(module$id)
      if (module$id == "rep_markdown"){
        return <- do.call(get(module$server_function), args = list(id = module$id, common = common, parent_session = session, COMPONENT_MODULES))
      } else {
        return <- do.call(get(module$server_function), args = list(id = module$id, common = common, parent_session = session, map = map))
      }
      if (is.list(return) &&
          "save" %in% names(return) && is.function(return$save) &&
          "load" %in% names(return) && is.function(return$load)) {
        modules[[module$id]] <<- return
      }
    })
  })

  ######################## #
  ### MAPPING ####
  ######################## #

  map <- core_mapping_module_server("core_mapping", common, input, COMPONENT_MODULES)

  ################################
  ### SAVE / LOAD  ####
  ################################

  core_save_module_server("core_save", common, modules, COMPONENTS, input)
  core_load_module_server("core_load", common, modules, map, COMPONENT_MODULES, parent_session = session)

  ################################
  ### DEBUGGING ####
  ################################

  output$debug <- renderPrint({
    #browser()
    #print(pryr::mem_used())
    }) %>% bindEvent(input$debug_button)

  ################################
  ### EXPORT TEST VALUES ####
  ################################
  exportTestValues(common = common)
}
