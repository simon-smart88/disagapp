common_class <- R6::R6Class(
  classname = "common",
  public = list(
    shape = NULL,
    agg = NULL,
    covs = NULL,
    cov_sum = NULL,
    prep = NULL,
    fit = NULL,
    pred = NULL,
    map_layers = NULL,
    poly = NULL,
    logger = NULL,
    meta = NULL,
    countries = readRDS(system.file("ex/countries.rds", package="geodata")),
    selected_country = NULL,
    add_map_layer = function(new_names) {
      for (new_name in new_names){
        if (!(new_name %in% self$map_layers)){
          self$map_layers <- c(self$map_layers,new_name)
          invisible(self)
        }
      }
    }
  )
)
