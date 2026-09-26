# Connectivity scenario problem

Solve a prioritizr prioritization problem, by incorporating graph
connectivity of the features.

## Usage

``` r
connectivity_scenario(cost_raster, features_rasters = NULL, budget_perc,
                      pre_graphs, locked_in = NULL, locked_out = NULL)
```

## Arguments

- cost_raster:

  `SpatRaster` object used as cost for prioritization. Its coordinates
  must correspond to the input given at
  [preprocess_graphs](https://cadam00.github.io/priorCON/reference/preprocess_graphs.md).

- features_rasters:

  features `SpatRaster` object used for prioritization. Its coordinates
  must correspond to the input given at
  [preprocess_graphs](https://cadam00.github.io/priorCON/reference/preprocess_graphs.md).

- budget_perc:

  `numeric` value \\\[0,1\]\\. It represents the budget percentage of
  the cost to be used for prioritization.

- pre_graphs:

  output of
  [get_metrics](https://cadam00.github.io/priorCON/reference/get_metrics.md)
  function.

- locked_in:

  `SpatRaster` object used as locked in constraints, where these
  planning units are selected in the solution, e.g. current protected
  areas. For details, see  
  [`prioritizr::add_locked_in_constraints`](https://prioritizr.net/reference/add_locked_in_constraints.html).

- locked_out:

  `SpatRaster` object used as locked out constraints, where these
  planning units are excluded from the solution, e.g. not appropriate
  for protected areas. For details, see
  [`prioritizr::add_locked_out_constraints`](https://prioritizr.net/reference/add_locked_out_constraints.html).

## Details

A connectivity prioritization problem is created and solved using
prioritizr package. The solver used for solving the problems is the best
available on the computer, following the solver hierarchy of prioritizr.
By default, the highs package using the [HiGHS](https://highs.dev)
solver is downloaded during package installation.

Both features and connectivity rasters are min-max scaled before solving
the prioritization problem.

## Value

A list containing input for
[get_outputs](https://cadam00.github.io/priorCON/reference/get_outputs.md).
Additionally, the connectivity `SpatRaster` objects that are used in
prioritization are returned as well, before and after min-max scaling
(`original_connectivity_rast` and `normalized_connectivity_rast` objects
correspondingly).

## References

Hanson, Jeffrey O, Richard Schuster, Nina Morrell, Matthew
Strimas-Mackey, Brandon P M Edwards, Matthew E Watts, Peter Arcese,
Joseph Bennett, and Hugh P Possingham. 2025. prioritizr: Systematic
Conservation Prioritization in R.
<https://CRAN.R-project.org/package=prioritizr>.

Hanson JO, Schuster R, Strimas‐Mackey M, Morrell N, Edwards BPM, Arcese
P, Bennett JR, and Possingham HP. 2025. Systematic conservation
prioritization with the prioritizr R package. *Conservation Biology*,
39: e14376. [doi:10.1111/cobi.14376](https://doi.org/10.1111/cobi.14376)

Huangfu, Qi, and JA Julian Hall. 2018. Parallelizing the Dual Revised
Simplex Method. *Mathematical Programming Computation* 10 (1): 119–42.
[doi:10.1007/s12532-017-0130-5](https://doi.org/10.1007/s12532-017-0130-5)

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

cost_raster <- get_cost_raster()
features_rasters <- get_features_raster()

# Solve a prioritizr prioritization problem,
# by incorporating graph connectivity of the features
connectivity_solution <- connectivity_scenario(cost_raster=cost_raster,
features_rasters=features_rasters, budget_perc=0.1, pre_graphs=pre_graphs)
#> ℹ `add_max_wtd_sum_objective()` has severe limitations - use with caution.

# Plot solution raster
terra::plot(connectivity_solution$solution, main="Connectivity Solution")


# Plot normalized connectivity raster
terra::plot(connectivity_solution$normalized_connectivity_rast,
main="Normalized S-Core Connectivity raster")
```
