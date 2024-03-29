resourcePath <- system.file("shiny", "www", package = "disagapp")
shiny::addResourcePath("disagapp-res", resourcePath)

easy_print <- "https://rawgit.com/rowanwins/leaflet-easyPrint/gh-pages/dist/bundle.js"

tagList(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    script = file.path("disagapp-res", "js", "shinyjs-funcs.js"),
    functions = c("scrollLogger", "disableModule", "enableModule")
  ),
  navbarPage(
    theme = bslib::bs_theme(version = 3,
                            bootswatch = "spacelab"),
    id = "tabs",
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/styles.css", rel = "stylesheet"),
                tags$script(src = easy_print))
    ),
    title = img(src = "logo.png", height = "50", width = "50",
                style = "margin-top: -15px"),
    windowTitle = "Disagapp",
    tabPanel("Intro", value = "intro"),
    tabPanel("Response", value = "resp"),
    tabPanel("Covariates", value = "cov"),
    tabPanel("Aggregation", value = "agg"),
    tabPanel("Prepare", value = "prep"),
    tabPanel("Fit", value = "fit"),
    tabPanel("Predict", value = "pred"),
    tabPanel("Reproduce", value = "rep"),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://github.com/simon-smart88/disagapp/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: simon.smart@cantab.net" target="_blank">Send Email</a>')),
    tabPanel(NULL, icon = icon("power-off"), value = "_stopapp")
  ),
  tags$div(
    class = "container-fluid",
    fluidRow(
      column(
        4,
        wellPanel(
          conditionalPanel(
            "input.tabs == 'intro'",
            actionButton("debug_button", "debug"),
            includeMarkdown("Rmd/text_intro_tab.Rmd")
          ),
          # INCIDENCE DATA ####
          conditionalPanel(
            "input.tabs == 'resp'",
            div("Component: Response Data", class = "componentName"),
            help_comp_ui("respHelp"),
            radioButtons(
              "respSel", "Modules Available:",
              choiceNames = insert_modules_option_names("resp"),
              choiceValues = insert_modules_option_values("resp"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("resp")
          ),
          # COVARIATE DATA ####
          conditionalPanel(
            "input.tabs == 'cov'",
            div("Component: Covariate Data", class = "componentName"),
            help_comp_ui("covHelp"),
            radioButtons(
              "covSel", "Modules Available:",
              choiceNames = insert_modules_option_names("cov"),
              choiceValues = insert_modules_option_values("cov"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("cov")
          ),
          # AGGREGATION DATA ####
          conditionalPanel(
            "input.tabs == 'agg'",
            div("Component: Aggregation Data", class = "componentName"),
            help_comp_ui("aggHelp"),
            radioButtons(
              "aggSel", "Modules Available:",
              choiceNames = insert_modules_option_names("agg"),
              choiceValues = insert_modules_option_values("agg"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("agg")
          ),
          # PREPARE DATA ####
          conditionalPanel(
            "input.tabs == 'prep'",
            div("Component: Prepare Data", class = "componentName"),
            help_comp_ui("prepHelp"),
            radioButtons(
              "prepSel", "Modules Available:",
              choiceNames = insert_modules_option_names("prep"),
              choiceValues = insert_modules_option_values("prep"),
            ),
            tags$hr(),
            insert_modules_ui("prep")
          ),
          # FIT MODEL ####
          conditionalPanel(
            "input.tabs == 'fit'",
            div("Component: Fit Model", class = "componentName"),
            help_comp_ui("fitHelp"),
            radioButtons(
              "fitSel", "Modules Available:",
              choiceNames = insert_modules_option_names("fit"),
              choiceValues = insert_modules_option_values("fit"),
            ),
            tags$hr(),
            insert_modules_ui("fit")
          ),
          # MAKE PREDICTION ####
          conditionalPanel(
            "input.tabs == 'pred'",
            div("Component: Make Prediction", class = "componentName"),
            help_comp_ui("predHelp"),
            radioButtons(
              "predSel", "Modules Available:",
              choiceNames = insert_modules_option_names("pred"),
              choiceValues = insert_modules_option_values("pred"),
            ),
            tags$hr(),
            insert_modules_ui("pred")
          ),
          # REPRODUCIBILITY
          conditionalPanel(
            "input.tabs == 'rep'",
            div("Component: Reproduce", class = "componentName"),
            radioButtons(
              "repSel", "Modules Available:",
              choiceNames = insert_modules_option_names("rep"),
              choiceValues = insert_modules_option_values("rep"),
              selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("rep")
          )
        )
      ),
      # --- RESULTS WINDOW ---
      column(
        8,
        conditionalPanel(
          "input.tabs != 'intro' & input.tabs != 'rep'",
          fixedRow(
            column(
              2,
              offset = 1,
              align = "left",
              div(style = "margin-top: -10px"),
              strong("Log window"),
              div(style = "margin-top: 5px"),
              div(
                id = "messageLog",
                div(id = "logHeader", div(id = "logContent"))
              )
            )
          )
        ),
        br(),
        conditionalPanel(
          "input.tabs != 'intro' & input.tabs != 'rep'",
          tabsetPanel(
            id = "main",
            tabPanel(
              "Map",
              core_mapping_module_ui("core_mapping")
            ),
            tabPanel(
              "Results",
              lapply(COMPONENTS, function(component) {
                conditionalPanel(
                  glue::glue("input.tabs == '{component}'"),
                  insert_modules_results(component)
                )
              })
            ),
            tabPanel(
              "Component Guidance", icon = icon("circle-info"),
              uiOutput("gtext_component")
            ),
            tabPanel(
              "Module Guidance", icon = icon("circle-info", class = "mod_icon"),
              uiOutput("gtext_module")
            ),
            tabPanel(
              "Save", icon = icon("floppy-disk", class = "save_icon"),
              core_save_module_ui("core_save")
            )
          )
        ),
        ## save module data END ##
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == null",
          column(8,
                 includeMarkdown("Rmd/gtext_rep.Rmd")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_markdown'",
          column(8,
                 includeMarkdown("modules/rep_markdown.md")
          )
        ),
        conditionalPanel(
          "input.tabs == 'rep' & input.repSel == 'rep_refPackages'",
          column(8,
                 includeMarkdown("modules/rep_refPackages.md")
          )
        ),
        conditionalPanel(
          "input.tabs == 'intro'",
          tabsetPanel(
            id = "introTabs",
            tabPanel(
              "About",
              includeMarkdown("Rmd/text_about.Rmd")
            ),
            tabPanel(
              "Team",
              fluidRow(
                column(8, includeMarkdown("Rmd/text_team.Rmd")
                )
              )
            ),
            tabPanel(
              "How To Use",
              includeMarkdown("Rmd/text_how_to_use.Rmd")
            ),
            tabPanel(
              "Load Prior Session",
            core_load_module_ui("core_load")
            )
          )
        )
      )
    )
  )
)



