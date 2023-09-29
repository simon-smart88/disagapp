prep_prep_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run"), "Run module prep_prep")
  )
}

prep_prep_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####

    # TRIGGER
    gargoyle::trigger(prep_prep)
  })

  output$result <- renderText({
    # Result
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

prep_prep_module_result <- function(id) {
  ns <- NS(id)

  # Result UI
  verbatimTextOutput(ns("result"))
}

prep_prep_module_map <- function(map, common) {
  # Map logic
}

prep_prep_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    prep_prep_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

