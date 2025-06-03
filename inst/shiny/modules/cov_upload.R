cov_upload_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns("cov"),
              label = "Upload covariate data",
              multiple = TRUE,
              accept = c(".tif")),
    uiOutput(ns("example_out")),
    actionButton(ns("run"), "Upload file(s)", icon = icon("arrow-turn-down"))
  )
}

cov_upload_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  output$example_out <- renderUI({
    watch("resp_example")
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
        common$logger |> writeLog(type = "error", "Please select a raster file")
        return()
      }
      covdf <- input$cov
    }

    if (is.null(input$example) || (input$example == TRUE)){
      covdf <- data.frame(datapath = list.files(system.file("extdata", "covariates", package="disagapp"), full.names = TRUE),
                          name = list.files(system.file("extdata", "covariates", package="disagapp")))
    }

    if (length(common$covs_prep) > 0) {
      common$logger |> writeLog(type = "warning", "You will need to prepare the data
                                again to include the uploaded covariates in the model")
    }

    # FUNCTION CALL ####

    cov_list <- cov_upload(common$shape, covdf, common$logger)

    if (is.null(cov_list)){
      return()
    }

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
    common$meta$cov_upload$plot_height <- length(common$meta$cov_upload$path) * 400

    # TRIGGER
    do.call("cov_upload_module_map", list(map, common))
    trigger("cov_upload")
    common$logger |> writeLog(type = "complete", "Covariate data has been uploaded")
  })

  output$plot <- renderPlot({
    watch("cov_upload")
    req(common$meta$cov_upload)
    plot_raster(common$covs, common$meta$cov_upload$path)
  }, function(){ifelse(is.null(common$meta$cov_upload$plot_height), 400, common$meta$cov_upload$plot_height)})

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      example = input$example)
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      shinyWidgets::updateMaterialSwitch(session, "example", value = state$example)
    }
  ))
})
}

cov_upload_module_result <- function(id) {
  ns <- NS(id)
  plotOutput(ns("plot"))
}

cov_upload_module_map <- function(map, common) {
  for (variable in common$meta$cov_upload$path){
    raster_map(map, common, common$covs[[variable]], variable)
  }
}

cov_upload_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_upload_knit = !is.null(common$meta$cov_upload$used),
    cov_upload_path = common$meta$cov_upload$path,
    cov_upload_plot_height = common$meta$cov_upload$plot_height
  )
}

