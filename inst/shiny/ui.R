resourcePath <- system.file("shiny", "www", package = "disagapp")
addResourcePath("disagapp-res", resourcePath)

tagList(
  page_navbar(
    theme = bs_theme(version = 5,
                            bootswatch = "spacelab"),
    id = "tabs",
    header = tagList(
      rintrojs::introjsUI(),
      shinyjs::useShinyjs(),
      shinyjs::extendShinyjs(
        script = file.path("disagapp-res", "js", "shinyjs-funcs.js"),
        functions = c("scrollLogger", "disableModule", "enableModule", "runOnEnter")
      ),
      tags$link(href = "css/styles.css", rel = "stylesheet")
    ),
    title = img(src = "logo.png", height = "50", width = "50"),
    window_title = "Disagapp",
    nav_panel("Intro", value = "intro"),
    nav_panel("Response", value = "resp"),
    nav_panel("Covariates", value = "cov"),
    nav_panel("Aggregation", value = "agg"),
    nav_panel("Prepare", value = "prep"),
    nav_panel("Fit", value = "fit"),
    nav_panel("Predict", value = "pred"),
    nav_panel("Reproduce", value = "rep"),
    nav_menu("Support", icon = icon("life-ring"),
               HTML('<a href="https://github.com/simon-smart88/disagapp/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: tim.lucas@le.ac.uk" target="_blank">Send Email</a>')),
    if (Sys.getenv("leicester_server") == "") {
    nav_panel(NULL, icon = icon("power-off"), value = "_stopapp")
    }
  ),
  layout_sidebar(
    sidebar = sidebar(
      width = 400,
      open = "always",
      conditionalPanel(
        "input.tabs == 'intro'",
        includeMarkdown("Rmd/text_intro_tab.Rmd")
      ),
      insert_modules_ui("resp", "Response Data"),
      insert_modules_ui("cov", "Covariate Data"),
      insert_modules_ui("agg","Aggregation Data"),
      insert_modules_ui("prep", "Prepare Data"),
      insert_modules_ui("fit", "Fit Model"),
      insert_modules_ui("pred", "Make Prediction"),
      insert_modules_ui("rep", "Reproduce")
    ),
    # --- RESULTS WINDOW ---
    conditionalPanel(
      "input.tabs != 'intro'",
      layout_columns(
        col_widths = c(-3, 6, -3),
        div(
          div(style = "margin-top: -10px"),
          strong("Log window"),
          div(style = "margin-top: 5px"),
          div(
            id = "messageLog",
            div(id = "logHeader", div(id = "logContent"))
          ),
          br(),
          textOutput("running_tasks")
        )
      )
    ),
    conditionalPanel(
      "input.tabs != 'intro'",
      navset_tab(
        id = "main",
        nav_panel(
          "Map",
          core_mapping_module_ui("core_mapping")
        ),
        nav_panel(
          "Results",
          lapply(COMPONENTS, function(component) {
            conditionalPanel(
              glue::glue("input.tabs == '{component}'"),
              insert_modules_results(component)
            )
          })
        ),
        nav_panel(
          "Component guidance", icon = icon("circle-info"),
          flex_wrap(uiOutput("gtext_component"))
        ),
        nav_panel(
          "Module guidance", icon = icon("circle-info", class = "mod_icon"),
          flex_wrap(uiOutput("gtext_module"))
        ),
        nav_panel(
          "Save", icon = icon("floppy-disk", class = "save_icon"),
          core_save_module_ui("core_save")
        )
      )
    ),
    conditionalPanel(
      "input.tabs == 'intro'",
      flex_wrap(
      navset_tab(
        id = "introTabs",
        nav_panel(
          "About",
          core_intro_module_ui("core_intro"),
          # suppress logo path warnings
          suppressWarnings(includeMarkdown("Rmd/text_about.Rmd"))
        ),
        nav_panel(
          "Team",
          includeMarkdown("Rmd/text_team.Rmd")
        ),
        nav_panel(
          "How to use",
          includeMarkdown("Rmd/text_how_to_use.Rmd")
        ),
        nav_panel(
          "Load prior session",
          core_load_module_ui("core_load")
        )
      )
    ))
  )
)
