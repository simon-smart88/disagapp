#' Register a disagapp module
#'
#' Before running the disagapp application with \code{run_smart()}, you can
#' register your own modules to be used in disagapp.
#'
#' @param config_file The path to a YAML file that contains the information about
#' one or more modules.
#' @seealso \code{\link[disagapp]{create_module}}
#' @export
register_module <- function(config_file) {
  full_path <- NULL
  tryCatch({
    full_path <- normalizePath(path = config_file, mustWork = TRUE)
  }, error = function(e) {})

  if (is.null(full_path)) {
    stop("Cannot find the given file: ", config_file, call. = FALSE)
  }
  if (tools::file_ext(full_path) != "yml") {
    stop("The provided file is not a YAML file: ", config_file, call. = FALSE)
  }

  new_paths <- unique(c(getOption("disagapp_module_configs"), full_path))
  options("disagapp_module_configs" = new_paths)
}

#' Create a disagapp module
#'
#' Create the template of a new disagapp module.
#'
#' @param id The id of the module.
#' @param dir A directory where the new module should be created.
#' @param map Whether or not the module should support modifying the map.
#' @param result Whether or not the module should support showing information in
#' the Result tab.
#' @param rmd Whether or not the module should add Rmd code to the Session Code
#' download.
#' @param save Whether or not the module has some custom data to save when the
#' user saves the current session.
#' @seealso \code{\link[disagapp]{register_module}}
#' @export
create_module <- function(id, dir, map = FALSE, result = FALSE, rmd = FALSE, save = FALSE) {
  if (!grepl("^[A-Za-z0-9_]+$", id)) {
    stop("The id can only contain English characters, digits, and underscores",
         call. = FALSE)
  }

  # Copy the simple skeleton files to the new module directory
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(system.file("module_skeleton", "skeleton.yml", package = "disagapp"),
            file.path(dir, glue::glue("{id}.yml")), overwrite = TRUE)
  file.copy(system.file("module_skeleton", "skeleton.md", package = "disagapp"),
            file.path(dir, glue::glue("{id}.md")), overwrite = TRUE)

  if (rmd) {
    file.copy(system.file("module_skeleton", "skeleton.Rmd", package = "disagapp"),
              file.path(dir, glue::glue("{id}.Rmd")), overwrite = TRUE)
  }

  #add the module ID
  rmd_file <- readLines(file.path(dir, glue::glue("{id}.Rmd")))
  rmd_file <- gsub("moduleID_knit", glue::glue("{id}_knit"), rmd_file)
  writeLines(rmd_file, file.path(dir, glue::glue("{id}.Rmd")))

  # Copy the R code file, use the correct ID in all functions, and remove any
  # functions that the user doesn't want to use in this module
  r_file <- readLines(system.file("module_skeleton", "skeleton.R", package = "disagapp"))
  r_file <- paste(r_file, collapse = "\n")
  if (!map) {
    r_file <- gsub("\n\\{\\{id}}_module_map <- function.*?}\n", "", r_file)
  }
  if (!result) {
    r_file <- gsub("\n\\{\\{id}}_module_result <- function.*?}\n", "", r_file)
    r_file <- gsub("\n *output\\$.*?})\n", "", r_file)
  }
  if (!rmd) {
    r_file <- gsub("\n\\{\\{id}}_module_rmd <- function.*?)\n}", "", r_file)

  }
  if (!save) {
    r_file <- gsub("\n *return\\(list\\(.*?))\n", "", r_file)
  }
  r_file <- gsub("\\{\\{id}}", id, r_file)
  writeLines(r_file, file.path(dir, glue::glue("{id}.R")))

  message(glue::glue("Template for module `{id}` successfully created at ",
                     "`{normalizePath(dir)}`.\nDon't forget to call ",
                     "`disagapp::register_module(\"{dir}/{id}.yml\")` before running ",
                     "the app to add your module to disagapp."))
  invisible()
}
