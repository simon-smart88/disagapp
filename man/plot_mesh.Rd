% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_functions.R
\name{plot_mesh}
\alias{plot_mesh}
\title{plot_mesh}
\usage{
plot_mesh(
  data,
  title,
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
  nx = 500,
  ny = 500,
  ...
)
}
\arguments{
\item{data}{An \code{fm_mesh_2d} object.}

\item{title}{Character to describe the plot}

\item{color}{A vector of scalar values to fill the mesh with colors.
The length of the vector mus correspond to the number of mesh vertices.
The alternative name \code{colour} is also recognised.}

\item{alpha}{A vector of scalar values setting the alpha value of the colors provided.}

\item{edge.color}{Color of the regular mesh edges.}

\item{edge.linewidth}{Line width for the regular mesh edges. Default 0.25}

\item{interior}{If TRUE, plot the interior boundaries of the mesh.}

\item{int.color}{Color used to plot the interior constraint edges.}

\item{int.linewidth}{Line width for the interior constraint edges. Default 0.5}

\item{exterior}{If TRUE, plot the exterior boundaries of the mesh.}

\item{ext.color}{Color used to plot the exterior boundary edges.}

\item{ext.linewidth}{Line width for the exterior boundary edges. Default 1}

\item{crs}{A CRS object supported by \code{\link[fmesher:fm_transform]{fmesher::fm_transform()}} defining the coordinate
system to project the mesh to before plotting.}

\item{mask}{A \code{SpatialPolygon} or \code{sf} polygon defining the region that is plotted.}

\item{nx}{Number of pixels in x direction (when plotting using the color parameter).}

\item{ny}{Number of pixels in y direction (when plotting using the color parameter).}

\item{...}{ignored arguments (S3 generic compatibility).}
}
\value{
ggplot object
}
\description{
Plot the spatial mesh. Forked from inlabru::gg.fm_mesh_2d
https://github.com/inlabru-org/inlabru/blob/53ac741a5dba72c2bd33706fda48a149f0d8d9a9/R/ggplot.R#L750
}
\examples{
mesh <- fmesher::fm_mesh_2d_inla(boundary = fmesher::fm_extensions(cbind(2, 1), convex = 1, 2))
plot_mesh(mesh, "Mesh")

}
