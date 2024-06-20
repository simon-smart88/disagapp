cov_upload_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("cov"),
              label = "Upload covariate data",
              multiple = TRUE,
              accept = c(".tif")),
    uiOutput(ns("example_out")),
    actionButton(ns("run"), "Upload file(s)")
  )
}

cov_upload_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$example_out <- renderUI({
    gargoyle::watch("resp_example")
    if (!is.null(common$meta$resp_example$dataset) && (common$meta$resp_example$dataset == "mad")){
      shinyWidgets::materialSwitch(session$ns("example"), "Use example data", value = TRUE, status = "success")
    }
  })

  observeEvent(input$run, {
    # WARNING ####

    # check all files are .tif

    if (is.null(input$example) || (input$example == FALSE)){
      # check a file is selected
      if (is.null(input$cov)) {
        common$logger %>% writeLog(type = "error", "Please select a raster file")
        return()
      }
      covdf <- input$cov
    }

    if (is.null(input$example) || (input$example == TRUE)){
      covdf <- data.frame(datapath = list.files(system.file("extdata/covariates", package="disagapp"), full.names = TRUE),
                          name = list.files(system.file("extdata/covariates", package="disagapp")))
    }

    # FUNCTION CALL ####

    cov_list <- cov_upload(covdf, common$shape, common$logger)

    if (is.null(cov_list)){
      return()
    }

    common$logger %>% writeLog("Covariates uploaded")
    # LOAD INTO COMMON ####
    common$covs <- append(common$covs, cov_list)

    # METADATA ####
    common$meta$cov_upload$used <- TRUE
    #prevent over-writing if the module has already been used
    if (is.null(common$meta$cov_upload$path)){
      common$meta$cov_upload$path <- as.vector(covdf$name)
    } else {
      common$meta$cov_upload$path <- c(common$meta$cov_upload$path, as.vector(covdf$name))
    }

    # TRIGGER
    do.call("cov_upload_module_map", list(map, common))
    gargoyle::trigger("cov_upload")
  })

  output$result <- renderPlot({
    plot(common$covs)
  })

  return(list(
    save = function() {
list(example = input$example)
    },
    load = function(state) {
shinyWidgets::updateMaterialSwitch(session, "example", value = state$example)
    }
  ))
})
}

cov_upload_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  plotOutput(ns("result"))
}

cov_upload_module_map <- function(map, common) {
  for (variable in common$meta$cov_upload$path){
    covariate_map(map, common, common$covs[[variable]], variable)
  }
}

cov_upload_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_upload_knit = !is.null(common$meta$cov_upload$used),
    cov_upload_path = printVecAsis(common$meta$cov_upload$path)
  )
}

