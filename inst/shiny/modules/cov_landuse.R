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

cov_landuse_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(input$uses)) {
      common$logger %>% writeLog(type = "error", "Please select the land use categories to download")
      return()
    }
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    show_loading_modal("Please wait while the data is loaded")
    land_use <- cov_landuse(common$shape, input$year, input$uses)
    # LOAD INTO COMMON ####
    common$covs <- append(common$covs, land_use)
    close_loading_modal()
    common$logger %>% writeLog("Land use data has been downloaded")
    # METADATA ####
    common$meta$cov_landuse$used <- TRUE
    common$meta$cov_landuse$uses <- input$uses
    common$meta$cov_landuse$year <- input$year
    # TRIGGER
    gargoyle::trigger("cov_landuse")
  })

  return(list(
    save = function() {
list(uses = input$uses,
year = input$year)
    },
    load = function(state) {
updateSelectInput(session, "uses", selected = state$uses)
updateSelectInput(session, "year", selected = state$year)
    }
  ))
})
}

cov_landuse_module_map <- function(map, common) {
  for (use in common$meta$cov_landuse$uses){
    land_use <- paste0(use,"_land_use")
    covariate_map(map, common, common$covs[[land_use]], land_use)
  }
}

cov_landuse_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    cov_landuse_knit = !is.null(common$meta$cov_landuse$used),
    cov_landuse_uses = printVecAsis(common$meta$cov_landuse$uses),
    cov_landuse_year = common$meta$cov_landuse$year
  )
}

