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

resp_simplify_module_server <- function(id, common, parent_session, map) {
  moduleServer(id, function(input, output, session) {

  observeEvent(input$run, {
    # WARNING ####
    if (is.null(common$shape)) {
      common$logger |> writeLog(type = "error", "Please upload response data first")
      return()
    }
    # FUNCTION CALL ####
    common$shape <- resp_simplify(common$shape, as.numeric(input$distance))

    # METADATA ####
    common$meta$resp_simplify$used <- TRUE
    common$meta$resp_simplify$distance <- as.numeric(input$distance)

    # TRIGGER
    gargoyle::trigger("resp_simplify")
    do.call("resp_simplify_module_map", list(map, common))
    common$logger |> writeLog(type = "complete", "The polygons have been simplified")
  })

  output$current_size <- renderText({
    gargoyle::watch("resp_shape")
    gargoyle::watch("resp_combine")
    gargoyle::watch("resp_download")
    gargoyle::watch("resp_example")
    gargoyle::watch("resp_edit")
    gargoyle::watch("resp_simplify")
    req(common$shape)
    glue::glue("{round((object.size(common$shape)/1024^2),2)} Mb")})

  return(list(
    save = function() {list(
      ### Manual save start
      ### Manual save end
      distance = as.numeric(input$distance))
    },
    load = function(state) {
      ### Manual load start
      ### Manual load end
      updateNumericInput(session, "distance", value = state$distance)
    }
  ))
})
}

resp_simplify_module_map <- function(map, common) {
  shape_map(map, common)
}

resp_simplify_module_rmd <- function(common) {
  # Variables used in the module's Rmd code
  list(
    resp_simplify_knit = !is.null(common$meta$resp_simplify$used),
    resp_simplify_distance = common$meta$resp_simplify$distance
  )
}

