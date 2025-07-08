####################### #
# MAPPING FUNCTIONS #
####################### #

#' @title shape_map
#' @description For internal use. Plot response data on the leaflet map
#' @param map The leafletProxy object to add the shape to
#' @param common The common data structure
#' @keywords internal
#' @export
shape_map <- function(map, common){
  response <- as.numeric(common$shape[[common$response_name]])
  ex <- as.vector(terra::ext(common$shape))
  common$add_map_layer("Response")
  pal <- colorBin("viridis", domain = response, bins = 9, na.color ="#00000000")
  map |>
    clearControls() |>
    removeLayersControl() |>
    clearGroup("Response") |>
    removeControl("Response") |>
    addPolygons(data = common$shape, fillColor = ~pal(response), color = "black", fillOpacity = 0.7, weight = 2, group = "Response", popup = ~as.character(round(response,0))) |>
    fitBounds(lng1 = ex[[1]], lng2 = ex[[2]], lat1 = ex[[3]], lat2 = ex[[4]]) |>
    addLegend(position = "bottomright", pal = pal, values = response, group = "Response", title = "Response", layerId = "Response") |>
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) |>
    hideGroup(common$map_layers) |>
    showGroup("Response")
}

#' @title raster_map
#' @description For internal use. Plot raster data on the leaflet map
#' @param map The leafletProxy object to add the raster to
#' @param common The common data structure
#' @param raster The SpatRaster to plot
#' @param name The name of the raster
#' @param log Whether to plot the raster using a log scale
#' @param selected The layer to display
#' @keywords internal
#' @export
raster_map <- function(map, common, raster, name, log = FALSE, selected = NULL){
  common$add_map_layer(name)
  if (log == TRUE){
    raster[terra::values(raster) == 0] <- NA
    raster = log10(raster)
    title = paste0(name, " (log 10)")
  } else {
    title = name
  }

  domain <- c(min(terra::values(raster), na.rm = T), max(terra::values(raster), na.rm = T))
  pal <- colorBin("plasma", domain = domain, bins = 9, na.color = "#00000000")

  map |>
    clearControls() |>
    removeLayersControl() |>
    clearGroup(name) |>
    removeControl(name) |>
    addRasterImage(raster, group = name, colors = pal) |>
    addLegend(position = "bottomleft", pal = pal, values = terra::values(raster), group = name, title = title, layerId = name) |>
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE))

  if (is.null(selected)){
    map |>
      hideGroup(common$map_layers) |>
      showGroup(name)
  } else {
    map |>
      hideGroup(common$map_layers) |>
      showGroup(selected)
  }
}

#' @title replot_raster_map
#' @description For internal use. Replot covariate data on the leaflet map after
#' changes in the selected set
#' @param map The leafletProxy object to replot the rasters on
#' @param common The common data structure
#' @param covariates character. The name of the item in common containing
#'  the covariates to replot
#' @param aggregation character. The name of the item in common containing
#'  the aggregation raster to replot
#' @param selected_layer character. The currently selected layer
#' @keywords internal
#' @export
replot_raster_map <- function(map, common, covariates, aggregation, selected_layer){
  for (layer in names(common[[covariates]])){
    if (layer == "Nighttime light"){module <- "cov_nightlight"}
    else if (layer == "Population density"){module <- "cov_worldpop"}
    else {module <- "NULL"}
    if (!is.null(common$meta[[module]]$log)){log <-  common$meta[[module]]$log} else {log = FALSE}
    raster_map(map, common, common[[covariates]][[layer]], layer, log = log, selected = selected_layer)
  }
  agg_log <- c(common$meta$agg_worldpop$log, common$meta$agg_upload$log, common$meta$agg_uniform$log)
  agg_layer <- names(common[[aggregation]])
  raster_map(map, common, common[[aggregation]], agg_layer, agg_log, selected = selected_layer)
  if (!(agg_layer %in% selected_layer)){
    map |>
      removeControl(agg_layer)
  }
}

#' @title mesh_map
#' @description For internal use. Plot spatial mesh on the leaflet map
#' @param map The leafletProxy object to add the mesh to
#' @param common The common data structure
#' @keywords internal
#' @export
mesh_map <- function(map, common){

  choice <- common$meta$prep_mesh$selected

  #convert the inla mesh to a format which leaflet can handle
  sf_mesh <- common$mesh[[choice]] |> fmesher::fm_as_sfc() |>  sf::st_as_sf() |> sf::st_zm()
  bbox <- sf::st_bbox(sf_mesh)

  common$add_map_layer("Mesh")

  map |>
    clearControls() |>
    removeLayersControl() |>
    clearGroup("Mesh") |>
    addPolylines(data = sf_mesh, stroke = "black", weight = 2 , fill = FALSE, group = "Mesh") |>
    fitBounds(lng1 = bbox[[1]], lng2 = bbox[[3]], lat1 = bbox[[2]], lat2 = bbox[[4]]) |>
    addLayersControl(overlayGroups = common$map_layers, options = layersControlOptions(collapsed = FALSE)) |>
    hideGroup(common$map_layers) |>
    showGroup("Mesh")
}
