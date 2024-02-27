resp_simplify_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$label("Current size:"),
    textOutput(ns("current_size")),
    br(),
    numericInput(ns("distance"), "Simplify distance (metres)", value = 100),
    actionButton(ns("run"), "Simplify")
  )
}

resp_simplify_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$shape)) {
      common$logger %>% writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    common$shape <- resp_simplify(common$shape, input$distance)

    # METADATA ####
    common$meta$resp_simplify$used <- TRUE
    common$meta$resp_simplify$distance <- input$distance

    # TRIGGER
    gargoyle::trigger("resp_simplify")
  })

  output$current_size <- renderText({
    gargoyle::watch("resp_shape")
    gargoyle::watch("resp_combine")
    gargoyle::watch("resp_download")
    gargoyle::watch("resp_edit")
    gargoyle::watch("resp_simplify")
    req(common$shape)
    glue::glue("{round((object.size(common$shape)/1024^2),2)} Mb")})

  return(list(
    save = function() {
list(distance = input$distance)
    },
    load = function(state) {
updateNumericInput(session, "distance", value = state$distance)
    }
  ))
})
}

resp_simplify_module_map <- function(map, common) {
  map %>%
    removeControl("Response")
  #find which meta response isn't NULL, return the first if more than one
  response_variable <- c(common$meta$resp_shape$response,
                         common$meta$resp_combine$response,
                         common$meta$resp_download$response)[1]
  response <- common$shape[[response_variable]]
  shape_map(map, common, response)
}

resp_simplify_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_simplify_knit = !is.null(common$meta$resp_simplify$used),
    resp_simplify_distance = common$meta$resp_simplify$distance
  )
}

