\name{connectivity_scenario}
\alias{connectivity_scenario}
\title{Connectivity scenario problem}
\usage{
connectivity_scenario(cost_raster, features_rasters = NULL, budget_perc,
                      pre_graphs, locked_in = NULL, locked_out = NULL)
}
\description{
Solve a \pkg{prioritizr} prioritization problem, by incorporating graph
connectivity of the features.
}
\arguments{
\item{cost_raster}{
\code{SpatRaster} object used as cost for prioritization.
Its coordinates must correspond to the input given at
\link[priorCON]{preprocess_graphs}.
}
\item{features_rasters}{
features \code{SpatRaster} object used for prioritization.
Its coordinates must correspond to the input given at
\link[priorCON]{preprocess_graphs}.
}
\item{budget_perc}{
\code{numeric} value \eqn{[0,1]}. It represents the budget percentage of the
cost to be used for prioritization.
}
\item{pre_graphs}{
output of \link[priorCON]{get_metrics} function.
}
\item{locked_in}{
\code{SpatRaster} object used as locked in constraints, where these planning
units are selected in the solution, e.g. current protected areas. For details,
see \cr \code{prioritizr::add_locked_in_constraints}.
}
\item{locked_out}{
\code{SpatRaster} object used as locked out constraints, where these planning
units are excluded from the solution, e.g. not appropriate for protected areas.
For details, see \code{prioritizr::add_locked_out_constraints}.
}
}

\details{

A connectivity prioritization problem is created and solved using
\pkg{prioritizr} package.
The solver used for solving the problems is the best available on the computer,
following the solver hierarchy of \pkg{prioritizr}. By default, the \pkg{highs}
package using the \href{https://highs.dev}{HiGHS} solver is downloaded during
package installation.

Both features and connectivity rasters are min-max scaled before solving the
prioritization problem.
}

\value{
A list containing input for \link[priorCON]{get_outputs}. Additionally, the
connectivity \code{SpatRaster} objects that are used in prioritization are
returned as well, before and after min-max scaling
(\code{original_connectivity_rast} and \code{normalized_connectivity_rast}
objects correspondingly).
}

\references{
Hanson, Jeffrey O, Richard Schuster, Nina Morrell, Matthew Strimas-Mackey,
Brandon P M Edwards, Matthew E Watts, Peter Arcese, Joseph Bennett, and Hugh P
Possingham. 2025. prioritizr: Systematic Conservation Prioritization in R.
\href{https://CRAN.R-project.org/package=prioritizr}{
https://CRAN.R-project.org/package=prioritizr}.

Hanson JO, Schuster R, Strimas‐Mackey M, Morrell N, Edwards BPM, Arcese P,
Bennett JR, and Possingham HP. 2025. Systematic conservation prioritization with
the prioritizr R package. \emph{Conservation Biology}, 39: e14376.
\doi{10.1111/cobi.14376}

Huangfu, Qi, and JA Julian Hall. 2018. Parallelizing the Dual Revised Simplex
Method. \emph{Mathematical Programming Computation} 10 (1): 119–42.
\doi{10.1007/s12532-017-0130-5}
}

\seealso{
\code{
\link[priorCON]{preprocess_graphs},
\link[priorCON]{get_metrics}
}
}

\examples{
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

# Plot solution raster
terra::plot(connectivity_solution$solution, main="Connectivity Solution")

# Plot normalized connectivity raster
terra::plot(connectivity_solution$normalized_connectivity_rast,
main="Normalized S-Core Connectivity raster")
}
