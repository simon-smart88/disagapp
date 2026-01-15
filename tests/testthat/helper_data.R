is_ci <- Sys.getenv("GITHUB_ACTIONS") == "true"

covdf <- data.frame(datapath = list.files(system.file("extdata", "covariates", package = "disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata", "covariates", package = "disagapp")))

shpdf <- data.frame(datapath = list.files(system.file("extdata", "shapes", package = "disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata", "shapes", package = "disagapp")))

aggdf <- data.frame(datapath = list.files(system.file("extdata", "aggregation", package = "disagapp"), full.names = TRUE),
                    name = list.files(system.file("extdata", "aggregation", package = "disagapp")))

df_path <- system.file("extdata", "lie.csv", package = "disagapp")

df <- read.csv(df_path)

ch_df <- data.frame("area" =  c('Aargau', 'Appenzell Ausserrhoden', 'Appenzell Innerrhoden',
           'Basel-Landschaft', 'Basel-Stadt', 'Bern', 'Fribourg', 'Genève',
           'Glarus', 'Graubünden', 'Jura', 'Luzern', 'Neuchâtel', 'Nidwalden',
           'Obwalden', 'Schaffhausen', 'Schwyz', 'Solothurn', 'St. Gallen',
           'Thurgau', 'Ticino', 'Uri', 'Valais', 'Vaud', 'Zug', 'Zürich'),
           "response" = 1:26)

mdf <- rbind(df, ch_df)

test_data_dir <- normalizePath(testthat::test_path("data"))

lie_shpdf <- data.frame(
  datapath = list.files(test_data_dir, pattern = "lie_shape", full.names = TRUE),
  name = list.files(test_data_dir, pattern = "lie_shape")
)

lie_shape <- sf::st_read(file.path(test_data_dir, "lie_shape.shp"))

shape <-  sf::st_read(file.path(test_data_dir, "mdg.shp"))

shpdf_small <- data.frame(datapath = list.files(test_data_dir, pattern = "mdg", full.names = TRUE),
                    name = list.files(test_data_dir, pattern = "mdg"))

country_code <- c("LIE", "CHE")
area_column <- "area"
resp_column <- "response"
admin_level <- "ADM1"

if (is_ci){
  save_path <- normalizePath(tempfile(fileext = ".rds"), winslash = "\\", mustWork = FALSE)
} else {
  save_path <- file.path(test_data_dir, "save_file.rds")
}

polygons <- list()
n_polygon_per_side <- 10
n_polygons <- n_polygon_per_side * n_polygon_per_side
n_pixels_per_side <- n_polygon_per_side * 2

for(i in 1:n_polygons) {
  row <- ceiling(i/n_polygon_per_side)
  col <- ifelse(i %% n_polygon_per_side != 0, i %% n_polygon_per_side, n_polygon_per_side)
  xmin = 2*(col - 1); xmax = 2*col; ymin = 2*(row - 1); ymax = 2*row
  polygons[[i]] <- list(cbind(c(xmin, xmax, xmax, xmin, xmin),
                              c(ymax, ymax, ymin, ymin, ymax)))
}

polys <- lapply(polygons, sf::st_polygon)
response_df <- data.frame(area_id = 1:n_polygons, response = runif(n_polygons, min = 0, max = 1000))
spdf <- sf::st_sf(response_df, geometry = polys)

# Create raster stack
r <- terra::rast(ncol=n_pixels_per_side, nrow=n_pixels_per_side)
terra::ext(r) <- terra::ext(spdf)
r[] <- sapply(1:terra::ncell(r), function(x) rnorm(1, ifelse(x %% n_pixels_per_side != 0, x %% n_pixels_per_side, n_pixels_per_side), 3))
r2 <- terra::rast(ncol=n_pixels_per_side, nrow=n_pixels_per_side)
terra::ext(r2) <- terra::ext(spdf)
r2[] <- sapply(1:terra::ncell(r), function(x) rnorm(1, ceiling(x/n_pixels_per_side), 3))
cov_stack <- c(r, r2)
names(cov_stack) <- c('layer1', 'layer2')


test_data <- disaggregation::prepare_data(polygon_shapefile = spdf,
                          covariate_rasters = cov_stack)

result <- disaggregation::disag_model(test_data,
                      field = TRUE,
                      iid = TRUE,
                      iterations = 100,
                      family = "poisson",
                      link = "log")

test_common <- list()
test_common$shape <- spdf
test_common$covs_prep <- cov_stack
test_common$agg_prep <- wrap_terra(r)
test_common$prep <- test_data
test_common$covs_prep <- wrap_terra(test_common$covs_prep)
test_common$prep$covariate_rasters <- wrap_terra(test_common$prep$covariate_rasters)
test_common$state$main$version = as.character(packageVersion("disagapp"))
test_common$state$main$app <- "disagapp"
test_common$meta$prep_final$used <- TRUE
class(test_common) <- "common"
if (is_ci){
  test_common_path <- normalizePath(tempfile(fileext = ".rds"), winslash = "\\", mustWork = FALSE)
} else {
  test_common_path <- file.path(test_data_dir, "test_common.rds")
}
saveRDS(test_common, test_common_path)

