# Detect graph communities for each biodiversity feature.

Detect graph communities for each biodiversity feature.

## Usage

``` r
get_metrics(connect_mat, which_community = "s_core", ...)
```

## Arguments

- connect_mat:

  a `data.frame` object where the edge lists are contained. See more in
  details.

- which_community:

  `character` value for community type detection. It can be one of
  `"s_core"`, `"louvain"`, `"walktrap"`, `"eigen"`, `"betw"`, `"deg"` or
  `"page_rank"`. The default is `"s_core"`.

- ...:

  Further arguments passed to the graph community detection algorithm.
  See details.

## Details

Function get_metrics is used to calculate graph metrics values. The edge
lists created from the previous step, or inserted directly from the user
are used in this step to create graphs. The directed graphs are
transformed to undirected. The function is based on the igraph package
which is used to create clusters using Louvain and Walktrap and
calculate the following metrics: Eigenvector Centrality, Betweenness
Centrality and Degree and PageRank. S-core is calculated using the
package brainGraph. Arguments added by `...` are passed to the
respective original functions
([`igraph::cluster_louvain`](https://r.igraph.org/reference/cluster_louvain.html),
[`igraph::cluster_walktrap`](https://r.igraph.org/reference/cluster_walktrap.html),
[`igraph::eigen_centrality`](https://r.igraph.org/reference/eigen_centrality.html),
[`igraph::strength`](https://r.igraph.org/reference/strength.html),
[`igraph::betweenness`](https://r.igraph.org/reference/betweenness.html),
[`igraph::page_rank`](https://r.igraph.org/reference/page_rank.html),
[`brainGraph::s_core`](https://rdrr.io/pkg/brainGraph/man/s_core.html)),
given that `connect_mat` is transformed to an undirected graph.

`connect_mat` is either the output of
[preprocess_graphs](https://cadam00.github.io/priorCON/reference/preprocess_graphs.md)
or a custom edge list `data.frame` object, with the following columns:

- `feature`: feature name.

- `from.X`: longitude of the origin (source).

- `from.Y`: latitude of the origin (source).

- `to.X`: longitude of the destination (target).

- `to.Y`: latitude of the destination (target).

- `weight`: connection weight.

## Value

A list containing input for
[basic_scenario](https://cadam00.github.io/priorCON/reference/basic_scenario.md)
or
[connectivity_scenario](https://cadam00.github.io/priorCON/reference/connectivity_scenario.md).

## See also

` `[`preprocess_graphs`](https://cadam00.github.io/priorCON/reference/preprocess_graphs.md)`, get_metrics `

## References

Csárdi, Gábor, and Tamás Nepusz. 2006. The Igraph Software Package for
Complex Network Research. *InterJournal Complex Systems*: 1695.
<https://igraph.org>.

Csárdi, Gábor, Tamás Nepusz, Vincent Traag, Szabolcs Horvát, Fabio
Zanini, Daniel Noom, and Kirill Müller. 2026. igraph: Network Analysis
and Visualization in R.
[doi:10.5281/zenodo.7682609](https://doi.org/10.5281/zenodo.7682609) .

Watson, Christopher G. 2026. brainGraph: Graph Theory Analysis of Brain
MRI Data.
[doi:10.32614/CRAN.package.brainGraph](https://doi.org/10.32614/CRAN.package.brainGraph)
.

## Examples

``` r
# Read connectivity files from folder and combine them
combined_edge_list <- preprocess_graphs(system.file("external",
                                        package="priorCON"),
                                        header = FALSE, sep =";")

# Set seed for reproducibility
set.seed(42)

# Detect graph communities using the s-core algorithm
pre_graphs <- get_metrics(combined_edge_list, which_community = "s_core")
```
