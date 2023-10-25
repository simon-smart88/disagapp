cov_landuse_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    selectInput(ns("uses"), "Land uses", multiple = TRUE,
                       choices = c("Bare", "BuiltUp", "Crops", "Grass",
                                    "MossLichen", "PermanentWater", "SeasonalWater",
                                  "Shrub", "Snow", "Tree")),
    selectInput(ns("year"), "Year", choices = 2015:2019, selected = 2019),
    actionButton(ns("run"), "Download data")
  )
}

cov_landuse_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(input$uses)) {
      common$logger %>% writeLog(type = "error", "Please select the land use categories to download")
      return()
    }
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload incidence data first")
      return()
    }
    # FUNCTION CALL ####
    showModal(modalDialog(title = "Info", "Please wait while the data is loaded.
                          This window will close once it is complete.", easyClose = FALSE))
    land_use <- cov_landuse(common$shape, input$year, input$uses)
    # LOAD INTO COMMON ####
    common$covs <- append(common$covs,land_use)
    removeModal()
    common$logger %>% writeLog("Land use data has been downloaded")
    # METADATA ####
    common$meta$landuse$used <- TRUE
    common$meta$landuse$uses <- input$uses
    common$meta$landuse$year <- input$year
    # TRIGGER
    gargoyle::trigger("cov_landuse")
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

cov_landuse_module_map <- function(map, common) {
  # Map logic
}

cov_landuse_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_landuse_knit = common$meta$landuse$used,
    cov_landuse_uses = printVecAsis(common$meta$landuse$uses),
    cov_landuse_year = common$meta$landuse$year
  )
}
