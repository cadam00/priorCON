# Changelog

## priorCON 0.1.8

### Minor changes

- Replaced the deprecated `add_max_utility_objective` function with its
  new name `add_max_wtd_sum_objective`, complying with `prioritizr`
  version 9.0.1 ([\#2](https://github.com/cadam00/priorCON/issues/2)).

- Updated references.

## priorCON 0.1.7

CRAN release: 2025-11-03

### Minor changes

- Faster `preprocess_graphs`, `get_outputs` and
  `graph_connectivity_rasters`.

- Minor corrections in manual.

## priorCON 0.1.6

CRAN release: 2025-08-19

### Major changes

- Add `graph_connectivity_rasters` function.

- Now `get_metrics` is able to take ellipsis (`...`) argument, passed to
  graph connectivity functions from `igraph` and `brainGraph`.

### Minor changes

- Update ‘tmap’ code from v3 to v4.

- Update Introduction.Rmd and README text.

## priorCON 0.1.5

CRAN release: 2025-04-20

### Minor changes

- Update ‘tmap’ code from v3 to v4.

- Add ‘prioritizr’ updated citation.

- Add connectivity raster outputs at `connectivity_solution` function.

- Update tests.

## priorCON 0.1.4

CRAN release: 2025-01-24

### Major changes

- Add `locked_in` and `locked_out` arguments in
  [`priorCON::basic_scenario`](https://cadam00.github.io/priorCON/reference/basic_scenario.md)
  and
  [`priorCON::connectivity_scenario`](https://cadam00.github.io/priorCON/reference/connectivity_scenario.md).

## priorCON 0.1.3

CRAN release: 2024-11-28

### Minor changes

- Update package authors in DESCRIPTION.

## priorCON 0.1.2

CRAN release: 2024-11-06

### Minor changes

- Remove redundant `r` SpatRaster object at
  [`terra::rasterize`](https://rspatial.github.io/terra/reference/rasterize.html)
  use (we care only for the “geometry” of the `r`, so no need for new
  object).

- Update Introduction.Rmd, README and DESCRIPTION text.

- Add pkgdown website.

## priorCON 0.1.1

CRAN release: 2024-09-07

### Minor changes

- Add `"page_rank"` option on `which_community` argument of
  `get_metrics`.

- Move figures used from README.md to man/figures.

- Update README.md and Introduction.Rmd text and add badges (CRAN
  version, developer version, R-CMD-check and codecov).

- Fix test-get_metrics.R for the upcoming ‘igraph’ releases, after
  changes on
  [`igraph::cluster_louvain`](https://r.igraph.org/reference/cluster_louvain.html),
  as noted by Szabolcs Horvát (for more see
  <https://github.com/cadam00/priorCON/issues/1>).

## priorCON 0.1.0

CRAN release: 2024-08-19

### Major changes

- Initial package version.
