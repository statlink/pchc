\name{Random values simulation from a Bayesian network}
\alias{rbn}
\title{
Random values simulation from a Bayesian network
}

\description{
Random values simulation from a Bayesian network.
}

\usage{
rbn(n, dagobj, x)
}

\arguments{
\item{n}{
The number of observations to generate.
}
\item{dagobj}{
A "bn" object. See the examples for more information.
}
\item{x}{
The data used to fit the Bayesian network in a data.frame format.
}
}

\details{
This information is taken directly from the R package "bnlearn". This function implements forward/logic sampling: values for the root nodes are sampled from their (un-conditional) distribution, then those of their children conditional on the respective parent sets. Thisis done iteratively until values have been sampled for all nodes.If "dagobj" contains NA parameter estimates (because of unobserved discrete parents configurations in the data the parameters were learned from), rbn will produce observations that contain NAs when thoseparents configurations appear in the simulated samples.
}

\value{
A data frame with the same structure (column names and data types) of the argument "data".
}

\references{
Korb K. and Nicholson A.E. (2010).Bayesian Artificial Intelligence. Chapman & Hall/CRC, 2nd edition.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{pchc} }
}

\examples{
# simulate a dataset with continuous data
x <- matrix( rnorm(200 * 20, 1, 10), nrow = 200 )
a <- pchc::pchc(x)
sim <- pchc::rbn( 100, dagobj = a$dag, x = x )
}
