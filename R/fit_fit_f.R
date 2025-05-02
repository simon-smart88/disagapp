#' @title Fit a disaggregation model
#' @description
#' This function is called by the fit_fit module and calls
#' disaggregation::disag_model() with the only difference being that covariates
#' are wrapped to enable asynchronous operation and any errors are returned as
#' a character
#'
#' @param data disag_data.  Object returned by prepare_data function that
#' contains all the necessary objects for the model fitting
#' @param priors list. List of prior values
#' @param family character. Model family to fit
#' @param link character. Link function to use
#' @param iterations numeric. Number of iterations
#' @param field logical. Whether to include a spatial field
#' @param iid  logical. Whether to include an iid effect
#' @param async logical. Whether or not the function is being used asynchronously
#' @return a list of class `disag_model` or a character string if an error occurs
#' @examples
#' \dontrun{
#' polygons <- list()
#' n_polygon_per_side <- 10
#' n_polygons <- n_polygon_per_side * n_polygon_per_side
#' n_pixels_per_side <- n_polygon_per_side * 2
#'
#' for(i in 1:n_polygons) {
#'   row <- ceiling(i/n_polygon_per_side)
#'   col <- ifelse(i %% n_polygon_per_side != 0, i %% n_polygon_per_side, n_polygon_per_side)
#'   xmin = 2*(col - 1); xmax = 2*col; ymin = 2*(row - 1); ymax = 2*row
#'   polygons[[i]] <- list(cbind(c(xmin, xmax, xmax, xmin, xmin),
#'                               c(ymax, ymax, ymin, ymin, ymax)))
#' }
#'
#' polys <- lapply(polygons,sf::st_polygon)
#' N <- floor(runif(n_polygons, min = 1, max = 100))
#' response_df <- data.frame(area_id = 1:n_polygons, response = runif(n_polygons, min = 0, max = 1000))
#'
#' spdf <- sf::st_sf(response_df, geometry = polys)
#'
#' # Create raster stack
#' r <- terra::rast(ncol=n_pixels_per_side, nrow=n_pixels_per_side)
#' terra::ext(r) <- terra::ext(spdf)
#' r[] <- sapply(1:terra::ncell(r), function(x){
#' rnorm(1, ifelse(x %% n_pixels_per_side != 0, x %% n_pixels_per_side, n_pixels_per_side), 3)})
#' r2 <- terra::rast(ncol=n_pixels_per_side, nrow=n_pixels_per_side)
#' terra::ext(r2) <- terra::ext(spdf)
#' r2[] <- sapply(1:terra::ncell(r), function(x) rnorm(1, ceiling(x/n_pixels_per_side), 3))
#' cov_stack <- c(r, r2)
#' names(cov_stack) <- c('layer1', 'layer2')
#'
#' test_data <- disaggregation::prepare_data(polygon_shapefile = spdf,
#'                           covariate_rasters = cov_stack)
#'
#' result <- fit_fit(test_data,
#'                   priors = NULL,
#'                   family = "gaussian"
#'                   link = "identity"
#'                   iterations = 2
#'                   field = TRUE,
#'                   iid = FALSE)
#'  }
#'
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

fit_fit <- function(data, priors, family, link, iterations, field, iid, async = FALSE){

  if (!inherits(data, "disag_data")){
    return(async |> asyncLog(type = "error", "data must be a disag_data object"))
  }

  if (!is.null(priors) && !inherits(priors, "list")){
    return(async |> asyncLog(type = "error", "priors must be a list object"))
  }

  if (!inherits(family, "character")){
    return(async |> asyncLog(type = "error", "family must be a character string"))
  }

  if (!inherits(link, "character")){
    return(async |> asyncLog(type = "error", "link must be a character string"))
  }

  if (!inherits(iterations, "numeric")){
    return(async |> asyncLog(type = "error", "iterations must be numeric"))
  }

  if (!inherits(field, "logical")){
    return(async |> asyncLog(type = "error", "field must be either TRUE or FALSE"))
  }

  if (!inherits(iid, "logical")){
    return(async |> asyncLog(type = "error", "iid must be either TRUE or FALSE"))
  }

  if (async){
    data$covariate_rasters <- terra::unwrap(data$covariate_rasters)
  }

  result <- tryCatch({disaggregation::disag_model(data = data,
                                        priors = priors,
                                        family = family,
                                        link = link,
                                        iterations = iterations,
                                        field = field,
                                        iid = iid)
    },
                     error = function(x){paste0("An error occurred whilst fitting the model: ", x)},
                     warning = function(x){paste0("An error occurred whilst fitting the model: ", x)}
  )

  if (is.character(result)){
    return(result)
  }

  if (async){
    result$data$covariate_rasters <- terra::wrap(result$data$covariate_rasters)
  }

  result

}
