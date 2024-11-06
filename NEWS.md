# priorCON 0.1.2

## Minor changes

- Remove redundant `r` SpatRaster object at `terra::rasterize` use (we care only
for the "geometry" of the `r`, so no need for new object).

- Update Introduction.Rmd, README and DESCRIPTION text.

# priorCON 0.1.1

## Minor changes

- Add `"page_rank"` option on `which_community` argument of `get_metrics`.

- Move figures used from README.md to man/figures.

- Update README.md and Introduction.Rmd text and add badges (CRAN version,
developer version, R-CMD-check and codecov).

- Fix test-get_metrics.R for the upcoming 'igraph' releases, after changes on
`igraph::cluster_louvain`, as noted by Szabolcs Horvát (for more see
https://github.com/cadam00/priorCON/issues/1).

# priorCON 0.1.0

## Major changes

- Initial package version.
