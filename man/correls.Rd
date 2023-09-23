\name{Correlations}
\alias{correls}
\title{
Correlation between a vector and a set of variables
}

\description{
Correlation between a vector and a set of variables.
}

\usage{
correls(y, x, type = "pearson", rho = 0, a = 0.05)
}

\arguments{
\item{y}{
A numerical vector.
}
\item{x}{
A matrix with the data.
}
\item{type}{
The type of correlation you want. "pearson" and "spearman" are the two supported types because their standard error is easily calculated. For the "groupcorrels" you can also put "kendall" because no hypothesis test is performed in that function.
}
\item{rho}{
The value of the hypothesised correlation to be used in the hypothesis testing.
}
\item{a}{
The significance level used for the confidence intervals.
}
}

\details{
The functions uses the built-in function "cor" which is very fast and then includes confidence intervals and produces a p-value for the hypothesis test.
}

\value{
A matrix with 5 column; the correlation, the p-value for the hypothesis test that each of them is
eaqual to "rho", the test statistic and the \eqn{a/2\%} lower and upper confidence limits.
}

%\references{
%Tsagris M.T., Preston S. and Wood A.T.A. (2011). A data-based power transformation for compositional data. In Proceedings of the 4th Compositional Data Analysis Workshop, Girona, Spain.
%}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}


\seealso{
\code{\link{corpairs}, \link{cortest}, \link{pcor}
}
}

\examples{
x <- matrix( rnorm(100 * 50 ), ncol = 50)
y <- rnorm(100)
r <- cor(y, x)  ## correlation of y with each of the xs
b <- correls(y, x)
}



