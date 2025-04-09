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
  graphics::par(mfrow = c(n_rasters, 2), oma = c(0, 0, 0, 0), mar= c(2, 2, 2, 1))

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
    dplyr::mutate(type = ifelse(.data$parameter %in% covariate_names, "Slope", .data$type))

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
#' Make a scatterplot comparing the observed and predicted values
#' @param plot_data list. Result of `disaggregation::plot_disag_model_data()`
#' @return The plotly plot object
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

#' @title plot_resolution
#' @description
#' Make either a histogram or boxplot showing the
#' @param plot_type character. Either `histogram` or `boxplot`
#' @param covariates SpatRaster. The covariates
#' @param shape sf. The response data
#' @param scale character. Either `original` or `low`
#' @param original_resolution list. Containing the resolution of the `width` and `height` of the original data.
#' @return The plotly plot object
#' @export
#'
plot_resolution <- function(plot_type, covariates, shape, scale, original_resolution = NULL){

  pixels_per_poly <- terra::extract(covariates, shape) |>
                     dplyr::group_by(.data$ID) |>
                     dplyr::summarise(n_pixels = dplyr::n())

  if (plot_type == "Histogram"){
    plot <- plotly::plot_ly( x = pixels_per_poly$n_pixels,
                             type = "histogram",
                             histnorm = "frequency",
                             marker = list(color = "#0072B2"),
                             stroke = list(color = "black")) |>
      plotly::layout(xaxis = list(title = "Cells per polygon"),
                     yaxis = list(title = "Frequency"))
  }

  if (plot_type == "Boxplot"){
    plot <- plotly::plot_ly(x = pixels_per_poly$n_pixels,
                            type = "box",
                            fillcolor = "#0072B2",
                            line = list(color = "black"),
                            marker = list(color = "black"),
                            name = "") |>
      plotly::layout(xaxis = list(title = "Cells per polygon", showline = TRUE, zeroline = FALSE),
                     yaxis = list(title = "", showline = TRUE, zeroline = FALSE))
  }

  if (scale == "original"){
    annotation <- list(
      list(
        x = 1, y = 0.95, xref = "paper", yref = "paper",
        text = glue::glue("Original mean cells per polygon = {round(mean(pixels_per_poly$n_pixels), 2)}"),
        font = list(size = 16, color = "black"),
        showarrow = FALSE
      ),
      list(
        x = 1, y = 0.88, xref = "paper", yref = "paper",
        text = glue::glue("Original cell width (m) = {round(original_resolution$width, 0)}"),
        font = list(size = 16, color = "black"),
        showarrow = FALSE
      ),
      list(
        x = 1, y = 0.81, xref = "paper", yref = "paper",
        text = glue::glue("Original cell height (m) = {round(original_resolution$height, 0)}"),
        font = list(size = 16, color = "black"),
        showarrow = FALSE
      )
      )
  } else {
    annotation <- list(
      list(
        x = 1, y = 0.95, xref = "paper", yref = "paper",
        text = glue::glue("Low resolution mean cells per polygon = {round(mean(pixels_per_poly$n_pixels), 2)}"),
        font = list(size = 16, color = "black"),
        showarrow = FALSE
      )
    )
  }

  plot |> plotly::layout(annotations = annotation)
}
