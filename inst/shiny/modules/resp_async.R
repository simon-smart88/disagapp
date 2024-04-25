resp_async_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    actionButton(ns("run_a"), "Run task A"),
    actionButton(ns("run_b"), "Run task B"),
    textOutput(ns("result_a")),
    textOutput(ns("result_b"))
  )
}

resp_async_module_server <- function(id, common, parent_session) {
  moduleServer(id, function(input, output, session) {

  gargoyle::init("resp_async_a")
  gargoyle::init("resp_async_b")

  resp_async_a <- function(task_number){
    Sys.sleep(5)
    return(glue::glue("Task A {task_number} completed"))
  }

  resp_async_b <- function(task_number){
    Sys.sleep(5)
    return(glue::glue("Task B {task_number} completed"))
  }

  task_a <- ExtendedTask$new(function() common$controller$promise(mode = "one"))
  observe(if (task_a$status() != "running") task_a$invoke())

  task_b <- ExtendedTask$new(function() common$controller$promise(mode = "one"))
  observe(if (task_b$status() != "running") task_b$invoke())


  observeEvent(input$run_a, {
    task_number <- input$run_a
    common$logger %>% writeLog(glue::glue("Starting task A {task_number}"))
    common$controller$push(command = resp_async_a(task_number),
                           data = list(resp_async_a = resp_async_a,
                                       task_number = task_number))
    common$logger %>% writeLog("Running tasks: ", common$controller$unresolved())
  })


  observeEvent(input$run_b, {
    task_number <- input$run_b
    common$logger %>% writeLog(glue::glue("Starting task B {task_number}"))
    common$controller$push(command = resp_async_b(task_number),
                           data = list(resp_async_b = resp_async_b,
                                       task_number = task_number))
    common$logger %>% writeLog("Running tasks: ", common$controller$unresolved())
  })

  observe({
    common$async_result_a <- task_a$result()$result[[1L]]
    common$logger %>% writeLog(common$async_result_a)
    common$logger %>% writeLog("Running tasks: ", common$controller$unresolved())
    gargoyle::trigger("resp_async_a")
  })

  observe({
    common$async_result_b <- task_b$result()$result[[1L]]
    common$logger %>% writeLog(common$async_result_b)
    common$logger %>% writeLog("Running tasks: ", common$controller$unresolved())
    gargoyle::trigger("resp_async_b")
  })

  output$result_a <- renderText({
    gargoyle::watch("resp_async_a")
    common$async_result_a
  })

  output$result_b <- renderText({
    gargoyle::watch("resp_async_b")
    common$async_result_b
  })

  })

}

