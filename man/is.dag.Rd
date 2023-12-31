\name{Check whether a directed graph is acyclic}
\alias{is.dag}
\title{
Check whether a directed graph is acyclic
}

\description{
Check whether a directed graph is acyclic.
}

\usage{
is.dag(dag)
}

\arguments{
\item{dag}{
A square matrix representing a directed graph which contains either 0 or 1, where G[i, j] = 1, means there is an arrow from node i to node j.
}
}

\details{
The topological sort is performed. If it cannot be performed, NAs are returned. Hence, the functions checks for NAs.
}

\value{
A logical value, TRUE if the matrix represents a DAG and FALSE otherwise.
}

\references{
Chickering D.M. (1995). A transformational characterization of equivalent Bayesian network structures.
Proceedings of the 11th Conference on Uncertainty in Artificial Intelligence, Montreal, Canada, 87--98.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{pchc}, \link{fedhc}, \link{mmhc}
}
}

\examples{
G <- pchc::rbn3(100, 20, 0.3)$G
pchc::is.dag(G)  ## TRUE
}
