resp_async_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    # UI
    bslib::input_task_button(ns("run_a"), "Run task A"),
    bslib::input_task_button(ns("run_b"), "Run task B"),
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

  task_a <- ExtendedTask$new(function(x) {
    promises::future_promise({
      resp_async_a(x)
    })
  }) |> bslib::bind_task_button("run_a")

  task_b <- ExtendedTask$new(function(x) {
    promises::future_promise({
      resp_async_b(x)
    })
  }) |> bslib::bind_task_button("run_b")


  observeEvent(input$run_a, {
    task_a$invoke(as.integer(input$run_a))
    common$logger %>% writeLog(glue::glue("Starting task A {as.integer(input$run_a)}"))
    obs_a$resume()
  })

  observeEvent(input$run_b, {
    task_b$invoke(as.integer(input$run_b))
    common$logger %>% writeLog(glue::glue("Starting task B {as.integer(input$run_b)}"))
  })

  obs_a <- observe({
    common$async_result_a <- task_a$result()
    obs_a$suspend()
    common$logger %>% writeLog(common$async_result_a)
    gargoyle::trigger("resp_async_a")
  })

  observe({
    common$async_result_b <- task_b$result()
    common$logger %>% writeLog(common$async_result_b)
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

