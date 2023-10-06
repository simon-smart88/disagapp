cov_upload_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = NS(id,"cov"),
              label = "Upload covariate data",
              multiple = TRUE,
              accept = c('.tif')),
    actionButton(ns("run"), "Upload file")
  )
}

cov_upload_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    # check a file is selected
    if (is.null(input$cov)) {
      common$logger %>% writeLog(type = "error", "Please select a raster file")
      return()
    }

    # check all files are .tif


    # FUNCTION CALL ####
    covdf <- input$cov
    cov_list <- incid_user(covdf)

    # LOAD INTO COMMON ####
    common$covs <- append(common$covs,cov_list)

    # METADATA ####
    common$meta$cov$path <- covdf$name
    common$meta$cov$upload <- TRUE
    # TRIGGER
    gargoyle::trigger(cov_upload)
  })


  return(list(
    save = function() {
      # Save any values that should be saved when the current session is saved
    },
    load = function(state) {
      # Load
    }
  ))
})
}

cov_upload_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

cov_upload_module_map <- function(map, common) {
  observeEvent(gargoyle::watch("cov_upload"), {
    req(common$covs)
    common$add_map_layer(names(common$covs))
    for (s in 1:length(names(common$covs))){
      pal <- colorBin("YlOrRd", domain = values(common$covs[[s]]), bins = 9,na.color ="#00000000")
      map %>%
        clearGroup(names(common$covs)[s]) %>%
        addRasterImage(common$covs[[s]],group=names(common$covs)[s],colors = pal) %>%
        addLegend(position="bottomleft",pal=pal,values=values(common$covs[[s]]),group=names(common$covs)[s],title=names(common$covs)[s])
    }
    map %>%
      addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(common$map_layers[2:(length(common$map_layers)-1)])
  })
}

cov_upload_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_upload_knit = common$meta$cov$upload,
    upload_cov_path = common$meta$cov$path
  )
}

