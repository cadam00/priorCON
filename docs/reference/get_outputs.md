# Evaluate outputs

Evaluate outputs from
[basic_scenario](https://cadam00.github.io/priorCON/reference/basic_scenario.md)
or
[connectivity_scenario](https://cadam00.github.io/priorCON/reference/connectivity_scenario.md)
functions for a selected feature.

## Usage

``` r
get_outputs(solution, feature, pre_graphs, loose = FALSE, patch = FALSE)
```

## Arguments

- solution:

  output from
  [basic_scenario](https://cadam00.github.io/priorCON/reference/basic_scenario.md)
  or
  [connectivity_scenario](https://cadam00.github.io/priorCON/reference/connectivity_scenario.md)
  functions.

- feature:

  `character` with a single feature name used for plots.

- pre_graphs:

  output of
  [get_metrics](https://cadam00.github.io/priorCON/reference/get_metrics.md)
  function.

- loose:

  use loose or strict graph community connectivity definition. See more
  in details.

- patch:

  `logical` value. If `patch = TRUE`, then different colors can be used
  for each distinct patch at output plots.

## Details

Loose graph connectivity indicates the case where two protected nodes
(cells) can be considered connected, even if the between them cells are
not protected (thus not included in the solution), whereas strict
connectivity indicates the case where two protected cells can be
considered connected, only if they are cells between them that are also
protected. The default is `loose = FALSE`, indicating the use of the
strict connectivity definition.

## Value

A list containing the following items:

- tmap: tmap plot of the solution including connections.

- solution: terra `SpatRaster` object representing the prioritization
  solution.

- connections: sf `LINESTRING` object representing the preserved
  connections of the solution.

- connectivity_table: `data.frame` containing all feature names at the
  first column, the relative held percentages at the second column and
  the percentage of connections held at the third column.

## See also

` `[`basic_scenario`](https://cadam00.github.io/priorCON/reference/basic_scenario.md)`, `[`connectivity_scenario`](https://cadam00.github.io/priorCON/reference/connectivity_scenario.md)` `

## References

Hijmans, Robert J. 2024. terra: Spatial Data Analysis.
[doi:10.32614/CRAN.package.terra](https://doi.org/10.32614/CRAN.package.terra)
.

Pebesma, Edzer. 2018. Simple Features for R: Standardized Support for
Spatial Vector Data. *The R Journal* 10 (1): 439–46.
[doi:10.32614/RJ-2018-009](https://doi.org/10.32614/RJ-2018-009) .

Pebesma, Edzer, and Roger Bivand. 2023. Spatial Data Science: With
applications in R. *Chapman and Hall/CRC*.
[doi:10.1201/9780429459016](https://doi.org/10.1201/9780429459016)

## Examples

``` r
# Read connectivity files from folder and combine them
combined_edge_list <- preprocess_graphs(
                                    system.file("external", package="priorCON"),
                                    header = FALSE, sep =";")

# Set seed for reproducibility
set.seed(42)

# Detect graph communities using the s-core algorithm
pre_graphs <- get_metrics(combined_edge_list, which_community = "s_core")

cost_raster <- get_cost_raster()
features_rasters <- get_features_raster()

# Solve a prioritizr prioritization problem, by incorporating graph connectivity
# of the features
connectivity_solution <- connectivity_scenario(
                           cost_raster      = cost_raster,
                           features_rasters = features_rasters,
                           budget_perc      = 0.1,
                           pre_graphs       = pre_graphs
                         )
#> ℹ `add_max_wtd_sum_objective()` has severe limitations - use with caution.

# Get outputs from connectivity_scenario function for feature "f1"
connectivity_outputs <- get_outputs(solution   = connectivity_solution,
                                    feature    = "f1",
                                    pre_graphs = pre_graphs)

# Plot tmap
connectivity_outputs$tmap


# Print summary of features and connections held percentages for connectivity
# scenario
print(connectivity_outputs$connectivity_table)
```
