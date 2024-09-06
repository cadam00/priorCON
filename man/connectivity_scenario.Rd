\name{connectivity_scenario}
\alias{connectivity_scenario}
\title{Connectivity scenario problem}
\usage{
connectivity_scenario(cost_raster, features_rasters = NULL, budget_perc,
                      pre_graphs)
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
}

\details{

A connectivity prioritization problem is created and solved using
\pkg{prioritizr} package.
The solver used for solving the problems is the best available on the computer,
following the solver hierarchy of \pkg{prioritizr}. By default, the \pkg{highs}
package using the \href{https://highs.dev/}{HiGHS} solver is downloaded during
package installation.
}

\value{
A list containing input for \link[priorCON]{get_outputs}.
}

\references{
Hanson, Jeffrey O, Richard Schuster, Nina Morrell, Matthew Strimas-Mackey,
Brandon P M Edwards, Matthew E Watts, Peter Arcese, Joseph Bennett, and Hugh P
Possingham. 2024. prioritizr: Systematic Conservation Prioritization in R.
\href{https://prioritizr.net}{https://prioritizr.net}.

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
}
