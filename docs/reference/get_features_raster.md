# Features raster example

Features raster example.

## Usage

``` r
get_features_raster()
```

## Value

A features `SpatRaster` object to use for examples.

## Examples

``` r
library(tmap)

## Import features_raster
features_raster <- get_features_raster()

## Plot with tmap
tm_shape(features_raster) +
  tm_raster(col.legend = tm_legend(title = "f1",
            position = c("right", "top")))
```
