\name{graph_connectivity_rasters}
\alias{graph_connectivity_rasters}
\title{Graph Connectivity Rasters}
\usage{
graph_connectivity_rasters(pu_raster, pre_graphs)
}
\description{
Graph connectivity rasters calculation.
}
\arguments{
\item{pu_raster}{
\code{SpatRaster} object used as planning units for maching its non-\code{NA}
cells with the coordinates described by the output of
\link[priorCON]{preprocess_graphs} (\code{pre_graphs}). Its coordinates must
correspond to the input given at \link[priorCON]{get_metrics}.
}
\item{pre_graphs}{
output of \link[priorCON]{get_metrics} function.
}
}

\details{

The graph connectivity per cell of \code{pu_raster} calculated by
\link[priorCON]{preprocess_graphs} is transformed to a single \code{SpatRaster}
object, where each layer corresponds to a different feature of
\code{pre_graphs}. This \code{pu_raster} is used as \code{SpatRaster} object in
\code{terra::rasterize} function and its exact non-\code{NA} values do not
matter, but only the fact that they are non-\code{NA}.

}

\value{
A \code{SpatRaster} object.
}


\seealso{
\code{
\link[priorCON]{preprocess_graphs},
\link[priorCON]{get_metrics}
}
}

\examples{
# Read connectivity files from folder and combine them
combined_edge_list <- preprocess_graphs(system.file("external", package="priorCON"),
                                        header = FALSE, sep =";")

# Set seed for reproducibility
set.seed(42)

# Detect graph communities using the s-core algorithm
pre_graphs <- get_metrics(combined_edge_list, which_community = "s_core")

# Planning Units SpatRaster object
pu_raster <- get_cost_raster()

# Get graph connectivity rasters
f1_s_core <- graph_connectivity_rasters(pu_raster, pre_graphs)

# Plot solution raster
terra::plot(f1_s_core, main="S-Core connectivity SpatRaster of f1")
}
