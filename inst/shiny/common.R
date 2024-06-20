common_class <- R6::R6Class(
  classname = "common",
  public = list(
    shape = NULL,
    response_name = NULL,
    agg = NULL,
    agg_prep = NULL,
    agg_prep_lores = NULL,
    covs = list(),
    covs_prep = NULL,
    covs_prep_lores = NULL,
    covs_matrix = NULL,
    covs_summary = NULL,
    mesh = NULL,
    prep = NULL,
    fit = NULL,
    pred = NULL,
    transfer = NULL,
    map_layers = NULL,
    poly = NULL,
    logger = NULL,
    meta = NULL,
    state = NULL,
    async_result_a = NULL,
    async_result_b = NULL,
    tasks = list(),
    seed = NULL,
    countries = readRDS(system.file("ex/countries.rds", package = "geodata")),
    selected_country = NULL,
    add_map_layer = function(new_names) {
      for (new_name in new_names){
        if (!(new_name %in% self$map_layers)){
          self$map_layers <- c(self$map_layers,new_name)
          invisible(self)
        }
      }
    },
    reset = function(){
      self$shape <- NULL
      self$response_name <- NULL
      self$agg <- NULL
      self$agg_prep <-NULL
      self$agg_prep_lores <- NULL
      self$covs <- list()
      self$covs_prep <- NULL
      self$covs_prep_lores <- NULL
      self$covs_matrix <- NULL
      self$covs_summary <- NULL
      self$mesh <- NULL
      self$prep <- NULL
      self$fit <- NULL
      self$pred <- NULL
      self$transfer <- NULL
      self$map_layers <- NULL
      self$poly <- NULL
      self$meta <- NULL
      self$state <- NULL
      self$selected_country <- NULL
      invisible(self)
    }
  )
)
