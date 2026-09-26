# Basic scenario problem

Solve an ordinary prioritizr prioritization problem.

## Usage

``` r
basic_scenario(cost_raster, features_rasters, budget_perc, locked_in = NULL,
locked_out = NULL)
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

A basic prioritization problem is created and solved using prioritizr
package. The solver used for solving the problems is the best available
on the computer, following the solver hierarchy of prioritizr. By
default, the highs package using the [HiGHS](https://highs.dev) solver
is downloaded during package installation.

Features and connectivity rasters are min-max scaled before solving the
prioritization problem.

## Value

A list containing input for
[get_outputs](https://cadam00.github.io/priorCON/reference/get_outputs.md).

## References

Hanson, Jeffrey O, Richard Schuster, Nina Morrell, Matthew
Strimas-Mackey, Brandon P M Edwards, Matthew E Watts, Peter Arcese,
Joseph Bennett, and Hugh P Possingham. 2026. prioritizr: Systematic
Conservation Prioritization in R.
<https://CRAN.R-project.org/package=prioritizr>.

Hanson JO, Schuster R, Strimas‐Mackey M, Morrell N, Edwards BPM, Arcese
P, Bennett JR, and Possingham HP. 2025, Systematic conservation
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

cost_raster <- get_cost_raster()
features_rasters <- get_features_raster()

# Solve an ordinary prioritizr prioritization problem
basic_solution <- basic_scenario(cost_raster=cost_raster,
features_rasters=features_rasters, budget_perc=0.1)
#> ℹ `add_max_wtd_sum_objective()` has severe limitations - use with caution.
#> 
#> ── Presolve checks ─────────────────────────────────────────────────────────────
#> 
#> ── Data limitation issues ──
#> 
#> ℹ These following issues indicate that solutions might not identify meaningful priority areas:
#> 
#> ✖ The problem only contains a single feature.
#> → Conservation planning generally requires multiple features (e.g., species, ecosystem types) to identify meaningful priority areas.
#> 
#> ── Results ──
#> 
#> ✖ Failed.
#> For more information, see `presolve_check()`.

# Plot solution raster
terra::plot(basic_solution$solution, main="Basic Solution")
```
