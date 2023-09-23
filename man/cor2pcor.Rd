\name{Partial correlation matrix from correlation or covariance matrix}
\alias{cor2pcor}
\title{
Partial correlation matrix from correlation or covariance matrix
}

\description{
Partial correlation matrix from correlation or covariance matrix.
}

\usage{
cor2pcor(R)
}

\arguments{
\item{R}{
A correlation or covariance matrix.
}
}

\details{
Given a correlation or covariance matrix the function will caclulate the pairwise partial correlation conditional on all other variables.
}

\value{
A matrix where each entry is the partial correlation matrix between each pair of variables conditional on all other variables.
}

%\references{
  %
  %}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
  %%  ~~further notes~~
    %}

\seealso{
\code{ \link{pcor}, \link{cortest}, \link{correls}
}
}

\examples{
x <- as.matrix(iris[, 1:4])
R <- cor(x)
cor2pcor(R)
pcor(R, 1, 2, 3:4, n = 150)
}

