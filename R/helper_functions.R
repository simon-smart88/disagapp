####################### #
# MISC #
####################### #
#' @title printVecAsis
#' @description For internal use. Print objects as character string
#' @param x object to print
#' @return A character string to reproduce the object
#' @keywords internal
#' @export
printVecAsis <- function(x) {
  if (is.numeric(x) && length(x) == 1){
    return(x)
  } else {
    utils::capture.output(dput(x))
  }
}

#' @title check_url
#' @description For internal use. Checks whether a URL is live
#' @param url character. The URL to check
#' @returns TRUE if url is live, FALSE if not
#' @keywords internal
#' @export
check_url <- function(url){
  req <- httr2::request(url)
  resp <- tryCatch(
    req %>% httr2::req_method("HEAD") %>% httr2::req_perform(),
    httr2_http_404 = function(cnd){NULL},
    httr2_failure = function(cnd){NULL},
    httr2_error = function(cnd){NULL}
  )
  if (is.null(resp)){
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' @title Spurious package call to avoid note of functions outside R folder
#' @description For internal use.
#' @param x x
#' @keywords internal
#' @import gargoyle leaflet.extras
#' @export
spurious <- function(x) {
  curl::curl(x)
  DT::renderDataTable(x)
  RColorBrewer::brewer.pal(x)
  R6::R6Class(x)
  corrplot::corrplot(x)
  disaggregation::build_mesh(x)
  fmesher::fm_as_sfc(x)
  geodata::worldclim_country(x)
  geosphere::centroid(x)
  leaflet.extras::removeDrawToolbar(x)
  markdown::html_format(x)
  openxlsx::read.xlsx(x)
  parallelly::as.cluster(x)
  plotly::plot_ly(x)
  promises::promise(x)
  renv::activate(x)
  rintrojs::introjs(x)
  rmarkdown::github_document(x)
  shinyWidgets::pickerInput(x)
  shinyjs::disable(x)
  SpatialEpi::bayes_cluster(x)
  zip::zipr(x)
  return()
}

#' @title reset_data_ui
#' @description For internal use. UI component that appears once response data has been loaded
#' @keywords internal
#' @param session The session object passed to function given to shinyServer.
#' @param common The common data structure
#' @export
reset_data_ui <- function(session, common){
  watch("resp_shape")
  watch("resp_combine")
  watch("resp_download")
  watch("resp_example")
  if (!is.null(common$shape)){
    shinyWidgets::materialSwitch(session$ns("reset"), "Delete existing data?", FALSE, status = "success")
  }
}

#' @title reset_data
#' @description For internal use. Clears the common structure of data, resets the map and any plots
#' @keywords internal
#' @param common The common data structure
#' @export
reset_data <- function(common){
  modules <- names(common$meta)
  common$reset()
  trigger("clear_map")
  for (module in modules){
    trigger(module)
  }
}

####################### #
# SHINY LOG #
####################### #

#' @title writeLog
#' @description For internal use. Add text to a logger
#' @param logger The logger to write the text to. Can be NULL or a function
#' @param ... Messages to write to the logger
#' @param type One of "default", "error", "warning"
#' @keywords internal
#' @export
writeLog <- function(logger, ..., type = "default") {
  if (is.null(logger)) {
    if (type == "error") {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == "warning") {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else if (is.function(logger)) {
    if (type == "default") {
      pre <- "> "
    } else if (type == "starting") {
      pre <- paste0(icon("clock", class = "log_start"), " ")
    } else if (type == "complete") {
      pre <- paste0(icon("check", class = "log_end"), " ")
    } else if (type == "info") {
      if (nchar(...) < 80){
        shinyalert::shinyalert(..., type = "info")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "info")
      }
      pre <- paste0(icon("info", class = "log_info"), " ")
    } else if (type == "error") {
      if (nchar(...) < 80){
        shinyalert::shinyalert(...,
                               type = "error")
      } else {
        shinyalert::shinyalert("Please, check Log window for more information ",
                               type = "error")
      }
      pre <- paste0(icon("xmark", class = "log_error"), " ")
    } else if (type == "warning") {
      if (nchar(...) < 80){
        shinyalert::shinyalert(...,
                               type = "warning")
      } else {
      shinyalert::shinyalert("Please, check Log window for more information ",
                             type = "warning")

      pre <- paste0(icon("triangle-exclamation", class = "log_warn"), " ")
      }
    }
    newEntries <- paste0("<br>", pre, ..., collapse = "")
    logger(paste0(logger(), newEntries))
  } else {
    warning("Invalid logger type")
  }
  invisible()
}

#' @title asyncLog
#' @description For internal use. Similar to writeLog but for use inside async
#' functions
#' @param async Whether the function is being used asynchronously
#' @param ... Messages to write to the logger
#' @param type One of `default`, `info`, `error`, `warning`
#' @returns No return value, called for side effects
#' @keywords internal
#' @export
asyncLog <- function(async, ..., type = "default"){
  if (!async) {
    if (type == "error") {
      stop(paste0(..., collapse = ""), call. = FALSE)
    } else if (type == "warning") {
      warning(paste0(..., collapse = ""), call. = FALSE)
    } else {
      message(paste0(..., collapse = ""))
    }
  } else {
    return(as.character(...))
  }
}

####################### #
# LOADING MODAL #
####################### #

#' @title show_loading_modal
#' @description For internal use. Show a modal when something is loading
#' @param message character. The message to display to the user.
#' @keywords internal
#' @export

show_loading_modal <- function(message){
  shinybusy::show_modal_spinner(
    spin = "self-building-square",
    color = "#446e9b",
    text = message
  )
}

#' @title close_loading_modal
#' @description For internal use. Close the modal once loading is complete
#' @param session The session object passed to function given to shinyServer.
#' @keywords internal
#' @export

close_loading_modal <- function (session = getDefaultReactiveDomain())
{
  session$sendModal("remove", NULL)
}

####################### #
# SELECTED COUNTRIES #
####################### #

#' @title country_out
#' @description For internal use. Produce a drop down list of countries and update all inputs once one country has been selected.
#' @param session The session object passed to function given to shinyServer.
#' @param common The common data structure
#' @keywords internal
#' @export
country_out <- function(session, common){
  init("country_out")
  renderUI({
    watch("country_out")
    if (is.null(common$selected_country)){
      selectInput(session$ns("country"), "Select country", c("", common$countries$boundaryName), multiple = TRUE)
    } else {
      selectInput(session$ns("country"), "Select country", common$countries$boundaryName, selected = common$selected_country, multiple = TRUE)
    }
  })
}

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
  agg_log <- c(common$meta$agg_worldpop$log, common$meta$agg_landuse$log, common$meta$agg_upload$log, common$meta$agg_uniform$log)
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

####################### #
# PLOTTING #
####################### #

#' @title plot_response
#' @description
#' Plot the response data as a histogram
#' @param response numeric. The response data
#' @export
#'

plot_response <- function(response){
  pal <- colorBin("plasma", domain = response, bins = 9, na.color ="#00000000")
  breaks <- base::pretty(response, n = 9)
  cols <- pal(breaks)

  plotly::plot_ly( x = response,
                   type = "histogram",
                   histnorm = "frequency",
                   xbins = list(start = min(breaks), end = max(breaks), size = diff(breaks)[1]),
                   marker = list(color = cols)) |>
    plotly::layout(xaxis = list(title = "Response"),
                   yaxis = list(title = "Frequency"))
}

#' @title plot_raster
#' @description
#' Plot rasters as a map and a histogram
#' @param rasters list. List of SpatRasters
#' @param raster_names character. Vector of names
#' @param bins numeric. The number of bins to split the data into. Default = 50
#' @param log logical. Whether to plot values on a log scale
#' @keywords internal
#' @export
#'
plot_raster <- function(rasters, raster_names, bins = 50, log = FALSE){

  col <- terra::map.pal("plasma", bins)
  n_rasters <- length(raster_names)
  par(mfrow = c(n_rasters, 2), oma = c(0, 0, 0, 0), mar= c(2, 2, 2, 1))

  for (r in raster_names){
    if (log){
      raster <- log10(rasters[[r]])
      minmax <- range(raster[is.finite(raster)])
    } else {
      raster <- rasters[[r]]
      minmax <- terra::minmax(raster)
    }

    # suppress sampling warning
    suppressWarnings(terra::hist(raster,
                breaks = seq(minmax[1], minmax[2], length.out = bins),
                freq = FALSE,
                col = col,
                xlab = "",
                cex.lab = 0.75,
                cex.axis = 0.75))
    terra::plot(raster, col = col, mar = NA)

  }
}

#' @title plot_mesh
#' @description Plot the spatial mesh. Forked from inlabru::gg.fm_mesh_2d
#' https://github.com/inlabru-org/inlabru/blob/53ac741a5dba72c2bd33706fda48a149f0d8d9a9/R/ggplot.R#L750
#' @param data An `fm_mesh_2d` object.
#' @param title Character to describe the plot
#' @param color A vector of scalar values to fill the mesh with colors.
#' The length of the vector mus correspond to the number of mesh vertices.
#' The alternative name `colour` is also recognised.
#' @param alpha A vector of scalar values setting the alpha value of the colors provided.
#' @param edge.color Color of the regular mesh edges.
#' @param edge.linewidth Line width for the regular mesh edges. Default 0.25
#' @param interior If TRUE, plot the interior boundaries of the mesh.
#' @param int.color Color used to plot the interior constraint edges.
#' @param int.linewidth Line width for the interior constraint edges. Default 0.5
#' @param exterior If TRUE, plot the exterior boundaries of the mesh.
#' @param ext.color Color used to plot the exterior boundary edges.
#' @param ext.linewidth Line width for the exterior boundary edges. Default 1
#' @param crs A CRS object supported by [fmesher::fm_transform()] defining the coordinate
#' system to project the mesh to before plotting.
#' @param nx Number of pixels in x direction (when plotting using the color parameter).
#' @param ny Number of pixels in y direction (when plotting using the color parameter).
#' @param mask A `SpatialPolygon` or `sf` polygon defining the region that is plotted.
#' @param ... ignored arguments (S3 generic compatibility).
#' @export

plot_mesh <- function(data, title,
                      color = NULL,
                      alpha = NULL,
                      edge.color = "darkgrey",
                      edge.linewidth = 0.25,
                      interior = TRUE,
                      int.color = "blue",
                      int.linewidth = 0.5,
                      exterior = TRUE,
                      ext.color = "black",
                      ext.linewidth = 1,
                      crs = NULL,
                      mask = NULL,
                      nx = 500, ny = 500,
                      ...) {

  if (is.null(color) && ("colour" %in% names(list(...)))) {
    color <- list(...)[["colour"]]
  }
  if (!is.null(color)) {
    px <- fmesher::fm_pixels(data, dims = c(nx, ny), mask = mask, format = "sf")
    proj <- fmesher::fm_evaluator(data, px)
    px$color <- fmesher::fm_evaluate(proj, field = color)
    if (!is.null(alpha)) {
      if (length(alpha) == 1) {
        px$alpha <- alpha
      } else {
        px$alpha <- fmesher::fm_evaluate(proj, field = alpha)
      }
      gg <- gg(px,
               ggplot2::aes(fill = .data[["color"]]),
               alpha = px[["alpha"]],
               geom = "tile"
      )
    } else {
      gg <- gg(px,
               ggplot2::aes(fill = .data[["color"]]),
               geom = "tile"
      )
    }

    return(gg)
  }

  if (data$manifold == "S2") {
    stop("Geom not implemented for spherical meshes (manifold = S2)")
  }
  if (!is.null(crs)) {
    data <- fmesher::fm_transform(data, crs = crs)
  }

  df <- rbind(
    data.frame(a = data$loc[data$graph$tv[, 1], c(1, 2)], b = data$loc[data$graph$tv[, 2], c(1, 2)]),
    data.frame(a = data$loc[data$graph$tv[, 2], c(1, 2)], b = data$loc[data$graph$tv[, 3], c(1, 2)]),
    data.frame(a = data$loc[data$graph$tv[, 1], c(1, 2)], b = data$loc[data$graph$tv[, 3], c(1, 2)])
  )

  colnames(df) <- c("x", "y", "xend", "yend")
  mp <- ggplot2::aes(x = .data$x, y = .data$y, xend = .data$xend, yend = .data$yend)
  msh <- ggplot2::geom_segment(data = df, mapping = mp, color = edge.color, linewidth = edge.linewidth)

  # Outer boundary
  if (exterior) {
    df <- data.frame(
      data$loc[data$segm$bnd$idx[, 1], 1:2],
      data$loc[data$segm$bnd$idx[, 2], 1:2]
    )
    colnames(df) <- c("x", "y", "xend", "yend")
    bnd <- ggplot2::geom_segment(data = df, mapping = mp, color = ext.color, linewidth = ext.linewidth)
    centre_x <- mean(range(df$x))
    y_range <- diff(range(df$y))
    max_y <- max(df$y) + (y_range * 0.1)
  } else {
    bnd <- NULL
  }

  if (interior) {
    # Interior boundary
    df <- data.frame(
      data$loc[data$segm$int$idx[, 1], 1:2],
      data$loc[data$segm$int$idx[, 2], 1:2]
    )
    colnames(df) <- c("x", "y", "xend", "yend")
    if (nrow(df) == 0) {
      int <- NULL
    } else {
      int <- ggplot2::geom_segment(data = df, mapping = mp, color = int.color, linewidth = int.linewidth)
    }
  } else {
    int <- NULL
  }

  # Return combined geomes
  combined_plot <- c(msh, bnd, int)

  ggplot2::ggplot() +
    combined_plot +
    ggplot2::coord_fixed() +
    ggplot2::theme_void() +
    ggplot2::annotate("text", x = centre_x, y = max_y, label = title, size = 6, hjust = 0.5)
}

#' @title plot_model
#' @description
#' Plot parameters of the fitted model
#' @param plot_data list. Result of `disaggregation::plot_disag_model_data()`
#' @param covariate_names character. Vector of the names of the covariates
#' @export
#'
plot_model <- function(plot_data, covariate_names){
  posteriors <- plot_data$posteriors |>
    dplyr::mutate(type = ifelse(parameter %in% covariate_names, "Slope", type))

  unique_types <- unique(posteriors$type)

  plots <- lapply(unique_types, function(type) {
    subset_data <- posteriors[posteriors$type == type, ]

    plotly::plot_ly(subset_data,
                    y = ~parameter,
                    x = ~mean,
                    type = "scatter",
                    mode = "markers",
                    marker = list(color = "black"),
                    error_x = list(array = ~sd, color = "blue")) |>
      plotly::layout(title = list(text = type, x = 0.5),
                     xaxis = list(title = "SD", showline = TRUE, zeroline = FALSE),
                     yaxis = list(title = "Parameter", showline = TRUE, zeroline = FALSE,
                                  range = c(-1, nrow(subset_data))),
                     margin = list(t = 100))
  })

  # Combine subplots into a single plot
  final_plot <- plotly::subplot(plots, nrows = 1, shareX = FALSE, margin = 0.05) |>
    plotly::layout(title = "Model parameters (excluding random effects)",
                   showlegend = FALSE)

  final_plot
}

#' @title plot_obs_pred
#' @description
#' Plot a comparison of the observed and predicted values
#' @param plot_data list. Result of `disaggregation::plot_disag_model_data()`
#' @export
#'
plot_obs_pred <- function(plot_data){
  data <- plot_data$data
  title <- plot_data$title

  x_range <- range(data$obs, data$pred)
  identity_line <- data.frame(x = x_range, y = x_range)

  obspred_plot <- plotly::plot_ly(data, x = ~obs, y = ~pred, type = "scatter", mode = "markers", name = "Including IID") |>
    plotly::add_trace(data = data, x = ~obs, y = ~pred_no_iid, type = "scatter", mode = "markers", name = "Excluding IID",
                      marker = list(color = "red")) |>
    plotly::add_lines(data = identity_line, x = ~x, y = ~y, line = list(color = "blue"), name = "1:1 line") |>
    plotly::layout(title = list(text = title, x = 0.5),
                   xaxis = list(title = "Observed", showline = TRUE, zeroline = FALSE),
                   yaxis = list(title = "Predicted", showline = TRUE, zeroline = FALSE),
                   margin = list(t = 100),
                   showlegend = TRUE)

  obspred_plot
}


####################### #
# CHANGING TABS #
####################### #

#' @title show_map
#' @description For internal use. Switches the view to the Map tab
#' @param parent_session Session object of the main server function
#' @keywords internal
#' @export
show_map <- function(parent_session){
  updateTabsetPanel(parent_session, "main", selected = "Map")
}

#' @title show_results
#' @description For internal use. Switches the view to the Results tab
#' @param parent_session Session object of the main server function
#' @keywords internal
#' @export
show_results <- function(parent_session){
  updateTabsetPanel(parent_session, "main", selected = "Results")
}


####################### #
# WRAP/UNWRAP TERRA #
####################### #

#' @title wrap_terra
#' @description For internal use. Flexible function for wrapping SpatRasters or
#' lists of SpatRasters but only when they exist.
#' @param object The SpatRaster or the list of SpatRasters to pack
#' @keywords internal
#' @export
wrap_terra <- function(object){
  if (!is.null(object) && (length(object) > 0)){
    if (inherits(object, "list")) {
      if (inherits(object[[1]], "SpatRaster")){
        object <- lapply(object, terra::wrap)
    }}
    if (inherits(object, "SpatRaster")){
      object <- terra::wrap(object)
    }
  }
  return(object)
}

#' @title unwrap_terra
#' @description For internal use. Flexible function for unwrapping SpatRasters or
#' lists of SpatRasters but only when they exist.
#' @param object The SpatRaster or the list of SpatRasters to pack
#' @keywords internal
#' @export
unwrap_terra <- function(object){
  if (!is.null(object) && (length(object) > 0)){
    if (inherits(object, "list")) {
      if (inherits(object[[1]], "PackedSpatRaster")){
        object <- lapply(object, terra::unwrap)
    }}
    if (inherits(object, "PackedSpatRaster")){
      object <- terra::unwrap(object)
    }
  }
  return(object)
}

