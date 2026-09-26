# Cost raster example

Cost raster example.

## Usage

``` r
get_cost_raster()
```

## Value

A cost `SpatRaster` object to use for examples.

## Examples

``` r
library(tmap)

## Import features_raster
cost_raster <- get_cost_raster()

## Plot with tmap
tm_shape(cost_raster) +
  tm_raster(col.legend = tm_legend(title = "cost",
            position = c("right", "top")))
#> The map variable `col` of the layer "raster" contains a unique value. Therefore
#> a discrete scale is applied (tm_scale_discrete).
```
