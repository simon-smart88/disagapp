cov_landuse_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    checkboxGroupInput(ns("uses"), "Land uses",
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

    # FUNCTION CALL ####
    land_use <- cov_landuse(common$shape, input$year, input$uses)
    # LOAD INTO COMMON ####
    common$covs <- append(common$covs,land_use)
    # METADATA ####

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
    cov_landuse_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

