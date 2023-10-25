incid_combo_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    fileInput(inputId = ns("spread"),
              label = "Upload incidence spreadsheet",
              multiple = FALSE,
              accept = c('.csv','.xlsx')),
    actionButton(ns("run"), "Run module incid_combo")
  )
}

incid_combo_module_server <- function(id, common) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####

    # FUNCTION CALL ####

    # LOAD INTO COMMON ####

    # METADATA ####

    # TRIGGER
    gargoyle::trigger("incid_combo")
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

incid_combo_module_map <- function(map, common) {
  # Map logic
}

incid_combo_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    incid_combo_knit = !is.null(common$some_object),
    var1 = common$meta$setting1,
    var2 = common$meta$setting2
  )
}

