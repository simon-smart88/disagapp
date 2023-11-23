resourcePath <- system.file("shiny", "www", package = "shinydisag")
shiny::addResourcePath("shiny-disag-res", resourcePath)

tagList(
  shinyjs::useShinyjs(),
  shinyjs::extendShinyjs(
    script = file.path("shiny-disag-res", "js", "shinyjs-funcs.js"),
    functions = c("scrollLogger", "disableModule", "enableModule")
  ),
  navbarPage(
    theme = bslib::bs_theme(version = 3,
                            bootswatch = "spacelab"),
    id = "tabs",
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/styles.css", rel = "stylesheet"))
    ),
    title = img(src = "logo.png", height = "50", width = "50",
                style = "margin-top: -15px"),
    windowTitle = "Shiny disag",
    tabPanel("Intro", value = "intro"),
    tabPanel("Incidence", value = "incid"),
    tabPanel("Covariates", value = "cov"),
    tabPanel("Aggregation", value = "agg"),
    tabPanel("Prepare", value = "prep"),
    tabPanel("Fit", value = "fit"),
    tabPanel("Predict", value = "pred"),
    tabPanel("Reproduce", value = "rep"),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://github.com/simon-smart88/shinyscholar/issues" target="_blank">GitHub Issues</a>'),
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
            includeMarkdown("Rmd/text_intro_tab.Rmd")
          ),
          # INCIDENCE DATA ####
          conditionalPanel(
            "input.tabs == 'incid'",
            div("Component: Incidence Data", class = "componentName"),
            help_comp_ui("incidHelp"),
            radioButtons(
              "incidSel", "Modules Available:",
              choices = insert_modules_options("incid")#,
              #selected = character(0)
            ),
            tags$hr(),
            insert_modules_ui("incid")
          ),
          # COVARIATE DATA ####
          conditionalPanel(
            "input.tabs == 'cov'",
            div("Component: Covariate Data", class = "componentName"),
            help_comp_ui("covHelp"),
            radioButtons(
              "covSel", "Modules Available:",
              choices = insert_modules_options("cov")#,
              #selected = character(0)
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
              choices = insert_modules_options("agg")#,
              #selected = character(0)
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
              choices = insert_modules_options("prep")
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
              choices = insert_modules_options("fit")
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
              choices = insert_modules_options("pred")
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
              choices = insert_modules_options("rep"),
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
              leaflet::leafletOutput("map", height = 700),
              absolutePanel(
                top = 160, right = 20, width = 150, draggable = TRUE,
                selectInput("bmap", "",
                            choices = c("ESRI Topo" = "Esri.WorldTopoMap",
                                        "Stamen Terrain" = "Stamen.Terrain",
                                        "Open Topo" = "OpenTopoMap",
                                        "ESRI Imagery" = "Esri.WorldImagery",
                                        "ESRI Nat Geo" = "Esri.NatGeoWorldMap"),
                            selected = "Esri.WorldTopoMap"
                )
              )
            ),
            # tabPanel(
            #   "Table", br(),
            #   DT::dataTableOutput("table"),
            #   downloadButton("dl_table", "CSV file")
            # ),
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
              br(),
              h5(em("Note: To save your session code or metadata, use the Reproduce component")),
              wellPanel(
                h4(strong("Save Session")),
                p(paste0("By saving your session into an RDS file, you can resume ",
                       "working on it at a later time or you can share the file",
                       " with a collaborator.")),
                shinyjs::hidden(p(
                  id = "save_warning",
                  icon("triangle-exclamation"),
                  paste0("The current session data is large, which means the ",
                         "downloaded file may be large and the download might",
                         " take a long time.")
                  )),

                verbatimTextOutput("common_covs"),
                downloadButton("save_session", "Save Session"),
                br()
              ),
              wellPanel(
                h4(strong("Download Data")),
                p(paste0("Download data/results from analyses from currently selected module")),
                ## save module data BEGIN ##
                # save histogram #
                # conditionalPanel(
                #   "input.plotSel == 'plot_hist'",
                #   br(),
                #   fluidRow(
                #     column(3, h5("Download histogram")),
                #     column(2, downloadButton('dl_hist', "PNG file"))
                #   )
                # )

              )
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
              h4("Load session"),
              includeMarkdown("Rmd/text_loadsesh.Rmd"),
              fileInput("load_session", "", accept = ".rds"),
              actionButton("goLoad_session", "Load RDS")
            )
          )
        )
      )
    )
  )
)



