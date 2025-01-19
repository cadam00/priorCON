\name{basic_scenario}
\alias{basic_scenario}
\title{Basic scenario problem}
\usage{
basic_scenario(cost_raster, features_rasters, budget_perc, locked_in = NULL,
locked_out = NULL)
}
\description{
Solve an ordinary \pkg{prioritizr} prioritization problem.
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
\item{locked_in}{
\code{SpatRaster} object used as locked in constraints, where these planning
units are selected in the solution, e.g. current protected areas. For details,
see \code{prioritizr::add_locked_in_constraints}.
}
\item{locked_out}{
\code{SpatRaster} object used as locked out constraints, where these planning
units are excluded from the solution, e.g. not appropriate for protected areas.
For details, see \code{prioritizr::add_locked_out_constraints}.
}
}

\details{

A basic prioritization problem is created and solved using \pkg{prioritizr}
package. The solver used for solving the problems is the best available on the
computer, following the solver hierarchy of \pkg{prioritizr}. By default, the
\pkg{highs} package using the \href{https://highs.dev}{HiGHS} solver is
downloaded during package installation.
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

cost_raster <- get_cost_raster()
features_rasters <- get_features_raster()

# Solve an ordinary prioritizr prioritization problem
basic_solution <- basic_scenario(cost_raster=cost_raster,
features_rasters=features_rasters, budget_perc=0.1)
}
