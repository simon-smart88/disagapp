rep_markdown_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # UI
    strong("Select download file type"),
    selectInput(ns("rmdFileType"), label = "",
                choices = c("Rmd" = ".Rmd", "PDF" = ".pdf", "HTML" = ".html", "Word" = ".docx")),
    uiOutput(ns("cov_chunks_out")),
    downloadButton(ns("dlRMD"), "Download Session Code")
  )
}

rep_markdown_module_server <- function(id, common, parent_session, COMPONENT_MODULES) {
  moduleServer(id, function(input, output, session) {

    output$cov_chunks_out <- renderUI({
      watch("rep_covs")
      if (!is.null(common$meta$rep_covs$used)){
        shinyWidgets::materialSwitch(session$ns("cov_chunks"), "Include covariate chunks?", FALSE, status = "success")
      }
    })

    output$dlRMD <- downloadHandler(
      filename = function() {
        paste0("disagapp-session-", Sys.Date(), input$rmdFileType)
      },
      content = function(file) {
        md_files <- c()

        rmd_intro_file <- tempfile(pattern = "intro_", fileext = ".Rmd")
        knit_params <- c(
          file = "Rmd/userReport_intro.Rmd",
          list(seed = common$seed)
        )
        intro_rmd <- do.call(knitr::knit_expand, knit_params)
        writeLines(intro_rmd, rmd_intro_file)

        md_intro_file <- tempfile(pattern = "intro_", fileext = ".md")
        rmarkdown::render(rmd_intro_file,
                          output_format = rmarkdown::github_document(html_preview = FALSE),
                          output_file = md_intro_file,
                          clean = TRUE,
                          encoding = "UTF-8")
        md_files <- c(md_files, md_intro_file)

        module_rmds <- NULL
        #force rep modules to beginning
        components <- names(COMPONENT_MODULES)
        components <- c("rep", components[components != c("rep")])
        for (component in components) {
          for (module in COMPONENT_MODULES[[component]]) {
            # print(module$id)
            rmd_file <- module$rmd_file
            rmd_function <- module$rmd_function
            if (is.null(rmd_file)) next

            if (is.null(rmd_function)) {
              rmd_vars <- list()
            } else {
              rmd_vars <- do.call(rmd_function, list(common))
            }

            # can't pass common$meta through dput
            if (module$id == "pred_transfer"){
              knit_params <- c(
                file = rmd_file,
                rmd_vars
              )
            } else {
              knit_params <- c(
                file = rmd_file,
                lapply(rmd_vars, printVecAsis)
              )
            }

            module_rmd <- do.call(knitr::knit_expand, knit_params)

            module_rmd_file <- tempfile(pattern = paste0(module$id, "_"),
                                        fileext = ".Rmd")
            writeLines(module_rmd, module_rmd_file)
            module_rmds <- c(module_rmds, module_rmd_file)
          }
        }

        module_rmds <- module_rmds[!grepl("rep_markdown", module_rmds)]

        #remove cov, agg and prep modules if rep_covs has been used
        if (!is.null(common$meta$rep_covs$used) && (input$cov_chunks == FALSE)){
          module_rmds <- module_rmds[!grepl("cov_", module_rmds)]
          module_rmds <- module_rmds[!grepl("agg_", module_rmds)]
        }
        if (!is.null(common$meta$rep_covs$post_prep) && (input$cov_chunks == FALSE)){
          module_rmds <- module_rmds[!grepl("prep_summary", module_rmds)]
          module_rmds <- module_rmds[!grepl("prep_resolution", module_rmds)]
        }

        module_md_file <- tempfile(pattern = paste0(module$id, "_"),
                                   fileext = ".md")
        rmarkdown::render(input = "Rmd/userReport_module.Rmd",
                          params = list(child_rmds = module_rmds),
                          output_format = rmarkdown::github_document(html_preview = FALSE),
                          output_file = module_md_file,
                          clean = TRUE,
                          encoding = "UTF-8")
        md_files <- c(md_files, module_md_file)

        combined_md <-
          md_files |>
          lapply(readLines) |>
          lapply(paste, collapse = "\n") |>
          paste(collapse = "\n\n")

        result_file <- tempfile(pattern = "result_", fileext = input$rmdFileType)
        if (input$rmdFileType == ".Rmd") {
          combined_rmd <- gsub("``` r", "```{r}", combined_md)
          combined_rmd <- unlist(strsplit(combined_rmd , "\n"))

          # add title section
          header <- c("---", paste0("title: ", combined_rmd[1]), "---")
          combined_rmd <- append(combined_rmd, header, after = 0)
          combined_rmd <- combined_rmd[-c(4, 5)]

          # convert chunk control lines
          chunk_control_lines <- grep("\\{r,", combined_rmd)
          if (length(chunk_control_lines) > 0){
            chunk_starts <- grep("```\\{r\\}", combined_rmd)
            chunks_to_remove <- NA
            for (i in seq_along(chunk_control_lines)) {
              chunks_to_remove[i] <- min(chunk_starts[chunk_starts > chunk_control_lines[i]])
            }
            combined_rmd <- combined_rmd[-chunks_to_remove]
            combined_rmd <- gsub("\\{r,", "```{r,", combined_rmd)
          }

          # fix any very long lines
          long_lines <- which(nchar(combined_rmd) > 4000)
          for (l in long_lines){
            split_lines <- strwrap(combined_rmd[l], 4000)
            combined_rmd <- combined_rmd[-l]
            combined_rmd <- append(combined_rmd, split_lines, l-1)
          }

          writeLines(combined_rmd, result_file, useBytes = TRUE)
        } else {
          combined_md_file <- tempfile(pattern = "combined_", fileext = ".md")
          writeLines(combined_md, combined_md_file)
          rmarkdown::render(
            input = combined_md_file,
            output_format =
              switch(
                input$rmdFileType,
                ".pdf" = rmarkdown::pdf_document(),
                ".html" = rmarkdown::html_document(),
                ".docx" = rmarkdown::word_document()
              ),
            output_file = result_file,
            clean = TRUE,
            encoding = "UTF-8"
          )
        }

        file.rename(result_file, file)
      }
    )

  } )}
