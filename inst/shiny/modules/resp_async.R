resp_async_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run_a"), "Run task A"),
    actionButton(ns("run_b"), "Run task B"),
    textOutput(ns("result_a"))
  )
}

resp_async_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  gargoyle::init("resp_async_a")

  resp_async_a <- function(){
    Sys.sleep(5)
    return(glue::glue("Task A completed"))
  }

  task_a <- ExtendedTask$new(function() common$controller$promise(mode = "one"))
  observe(if (task_a$status() != "running") task_a$invoke())

  observeEvent(input$run_a, {
    task_number <- input$run_a
    common$logger %>% writeLog(glue::glue("Starting task A {task_number}"))
    common$controller$push(command = resp_async_a(), data = list(resp_async_a = resp_async_a))
    common$logger %>% writeLog("Running tasks: ", common$controller$unresolved())
  })

  observe({
    common$async_result_a <- task_a$result()$result[[1L]]
    common$logger %>% writeLog(common$async_result_a)
    common$logger %>% writeLog("Running tasks: ", common$controller$unresolved())
    gargoyle::trigger("resp_async_a")
  })

  output$result_a <- renderText({
    gargoyle::watch("resp_async_a")
    common$async_result_a
  })




  })

}

