\name{Adjacency matrix of a Bayesian network}
\alias{bnmat}

\title{
Adjacency matrix of a Bayesian network
}

\description{Adjacency matrix of a Bayesian network.
}

\usage{
bnmat(dag)
}

\arguments{
\item{dag}{
A BN object, an object of class "bn".
}
}

\details{
The function is called from the "bnlearn" package which invokes the "Rgraphviz" package from Bioconductor and you need to install it first.
}

\value{
Adjacency matrix of a Bayesian network is extracted.
}

%\references{
%}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{pchc}, \link{mmhc}, \link{fedhc}
}
}

\examples{
\donttest{
x <- matrix( rnorm(200 * 10, 1, 10), nrow = 200 )
a <- pchc::pchc(x)
pchc::bnmat(a$dag)
}
}

