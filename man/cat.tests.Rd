\name{Chi-square and G-square tests of (unconditional) indepdence}
\alias{cat.tests}

\title{
Chi-square and G-square tests of (unconditional) indepdence
}

\description{Chi-square and G-square tests of (unconditional) indepdence.
}

\usage{
cat.tests(x, y, logged = FALSE)
}

\arguments{
\item{x}{
A numerical vector or a factor variable with data. The data must be consecutive numbers.
}
\item{y}{
A numerical vector or a factor variable with data. The data must be consecutive numbers.
}
\item{logged}{
Should the p-values be returned (FALSE) or their logarithm (TRUE)?
}
}

\details{
The function calculates the test statistic of the \eqn{X^2} and the \eqn{G^2} tests of unconditional
independence between x and y. x and y need not be numerical vectors like in \code{\link{g2test}}. This
function is more close to the spirit of MASS' loglm()" function which calculates both statistics
using Poisson log-linear models (Tsagris, 2017).
}

\value{
A matrix with two rows. In each row the \eqn{X^2} or \eqn{G^2} test statistic, its p-value and the degrees of freedom are returned.
}

\references{
Tsagris M. (2021). A new scalable Bayesian network learning algorithm with applications to economics.
Computational Economics, 57(1): 341--367.

Tsagris M. (2017). Conditional independence test for categorical data using Poisson log-linear model.
Journal of Data Science, 15(2): 347--356.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{g2test}, \link{correls}, \link{pchc.skel}
}
}

\examples{
x <- rbinom(100, 3, 0.5)
y <- rbinom(100, 2, 0.5)
cat.tests(x, y)
}

