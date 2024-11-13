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
#' @author Simon Smart <simon.smart@@cantab.net>
#' @export

fit_fit <- function(data, priors, family, link, iterations, field, iid, async = FALSE){
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
