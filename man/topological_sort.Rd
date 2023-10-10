\name{Topological sort of a Bayesian network}
\alias{topological_sort}
\title{
Topological sort of a Bayesian network
}

\description{
Topological sort of a Bayesian network.
}

\usage{
topological_sort(dag)
}

\arguments{
\item{dag}{
A square matrix representing a directed graph which contains 0s and 1s. If G[i, j] = 1 it
means there is an arrow from node i to node j. When there is no edge between nodes i and j if G[i, j] = 0.
}
}

\details{
The function is an R translation from an old matlab code.
}

\value{
A vector with numbers indicating the sorting. If the matrix does not correspond to a Bayesian network (or a DAG),
NA will be returned.
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
\code{ \link{bnplot}, \link{pchc}, \link{pchc.skel}
}
}

\examples{
y <- pchc::rbn3(100, 20, 0.2)
Gtrue <- y$G
a <- pchc::pchc(y$x)
G <- bnmat(a$dag)
topological_sort(G)
}

