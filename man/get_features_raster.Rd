\name{get_features_raster}
\alias{get_features_raster}
\title{Features raster example}
\usage{
get_features_raster()
}
\description{
Features raster example.
}

\value{
A features \code{SpatRaster} object to use for examples.
}

\examples{
library(tmap)

## Import features_raster
features_raster <- get_features_raster()

## Plot with tmap
tm_shape(features_raster) +
  tm_raster(title = "f1")
}