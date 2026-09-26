# Graph Connectivity Rasters

Graph connectivity rasters calculation.

## Usage

``` r
graph_connectivity_rasters(pu_raster, pre_graphs)
```

## Arguments

- pu_raster:

  `SpatRaster` object used as planning units for maching its non-`NA`
  cells with the coordinates described by the output of
  [preprocess_graphs](https://cadam00.github.io/priorCON/reference/preprocess_graphs.md)
  (`pre_graphs`). Its coordinates must correspond to the input given at
  [get_metrics](https://cadam00.github.io/priorCON/reference/get_metrics.md).

- pre_graphs:

  output of
  [get_metrics](https://cadam00.github.io/priorCON/reference/get_metrics.md)
  function.

## Details

The graph connectivity per cell of `pu_raster` calculated by
[preprocess_graphs](https://cadam00.github.io/priorCON/reference/preprocess_graphs.md)
is transformed to a single `SpatRaster` object, where each layer
corresponds to a different feature of `pre_graphs`. This `pu_raster` is
used as `SpatRaster` object in
[`terra::rasterize`](https://rspatial.github.io/terra/reference/rasterize.html)
function and its exact non-`NA` values do not matter, but only the fact
that they are non-`NA`.

## Value

A `SpatRaster` object.

## See also

` `[`preprocess_graphs`](https://cadam00.github.io/priorCON/reference/preprocess_graphs.md)`, `[`get_metrics`](https://cadam00.github.io/priorCON/reference/get_metrics.md)` `

## Examples

``` r
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
```
