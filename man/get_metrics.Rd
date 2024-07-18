\name{get_metrics}
\alias{get_metrics}
\title{Detect graph communities for each biodiversity feature.}
\usage{
get_metrics(connect_mat, which_community = "s_core")
}
\description{
Detect graph communities for each biodiversity feature.
}
\arguments{
\item{connect_mat}{
a \code{data.frame} object where the edge lists are contained. See more in
details.
}
\item{which_community}{
\code{character} value for community type detection. It can be one of \code{"s_core"},
\code{"louvain"}, \code{"walktrap"}, \code{"eigen"}, \code{"betw"} or \code{"deg"}.
The default is \code{"s_core"}.
}
}

\details{
Function \link[priorCOM]{get_metrics} is used to calculate graph metrics values.
The edge lists created from the previous step, or inserted directly from the user are used
in this step to create graphs. The directed graphs are transformed to undirected. The
function is based on the \pkg{igraph} package which is used to create clusters using
Louvain and Walktrap and calculate the following metrics: Eigenvector Centrality,
Betweenness Centrality and Degree. S-core is calculated using the package
\pkg{brainGraph}.

\code{connect_mat} is either the output of \link[priorCOM]{preprocess_graphs} or
a custom edge list \code{data.frame} object, with the following
columns:
\itemize{
\item{\code{feature}}: feature name.
\item{\code{from.X}}: longitude of the origin (source).
\item{\code{from.Y}}: latitude of the origin (source).
\item{\code{to.X}}: longitude of the destination (target).
\item{\code{to.Y}}: latitude of the destination (target).
\item{\code{weight}}: connection weight.
}
}

\value{
A list containing input for \link[priorCOM]{basic_scenario} or 
\link[priorCOM]{connectivity_scenario}.
}

\seealso{
\code{
\link[priorCOM]{preprocess_graphs},
\link[priorCOM]{get_metrics}
}
}

\references{
Csárdi, Gábor, and Tamás Nepusz. 2006. The Igraph Software Package for Complex
Network Research. \emph{InterJournal Complex Systems}: 1695.
\href{https://igraph.org}{https://igraph.org}.

Csárdi, Gábor, Tamás Nepusz, Vincent Traag, Szabolcs Horvát, Fabio Zanini,
Daniel Noom, and Kirill Müller. 2024. igraph: Network Analysis and Visualization
in R. \doi{10.5281/zenodo.7682609}.

Watson, Christopher G. 2024. brainGraph: Graph Theory Analysis of Brain MRI
Data. \doi{10.32614/CRAN.package.brainGraph}.
}

\examples{
# Read connectivity files from folder and combine them
combined_edge_list <- preprocess_graphs(system.file("external",
                                        package="priorCOM"),
                                        header = FALSE, sep =";")

# Set seed for reproducibility
set.seed(42)

# Detect graph communities using the s-core algorithm
pre_graphs <- get_metrics(combined_edge_list, which_community = "s_core")
}