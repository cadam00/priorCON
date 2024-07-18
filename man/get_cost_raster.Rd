\name{get_cost_raster}
\alias{get_cost_raster}
\title{Cost raster example}
\usage{
get_cost_raster()
}
\description{
Cost raster example.
}

\value{
A cost \code{SpatRaster} object to use for examples.
}

\examples{
library(tmap)

## Import features_raster
cost_raster <- get_cost_raster()

## Plot with tmap
tm_shape(cost_raster) +
  tm_raster(title = "cost")
}