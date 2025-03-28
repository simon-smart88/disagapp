#' Register a disagapp module
#'
#' Before running the disagapp application with \code{run_disagapp()}, you can
#' register your own modules to be used in disagapp. To create new modules use
#' \code{shinyscholar::create_module()}
#'
#' @param config_file The path to a YAML file that contains the information about
#' one or more modules.
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


