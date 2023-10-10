\name{Correlation significance testing using Fisher's z-transformation}
\alias{cortest}
\title{
Correlation significance testing using Fisher's z-transformation
}

\description{
Correlation significance testing using Fisher's z-transformation.
}

\usage{
cortest(y, x, rho = 0, a = 0.05 )
}

\arguments{
\item{y}{
A numerical vector.
}
\item{x}{
A numerical vector.
}
\item{rho}{
The value of the hypothesised correlation to be used in the hypothesis testing.
}
\item{a}{
The significance level used for the confidence intervals.
}
}

\details{
The function uses the built-in function "cor" which is very fast, then computes a confidence interval
and produces a p-value for the hypothesis test.
}

\value{
A vector with 5 numbers; the correlation, the p-value for the hypothesis test that each of them is
equal to "rho", the test statistic and the $a/2\%$ lower and upper confidence limits.

}

\references{
Tsagris M. (2021). A new scalable Bayesian network learning algorithm with applications to economics.
Computational Economics, 57(1): 341--367.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}


\seealso{
\code{\link{pcor}, \link{correls}, \link{corpairs}
}
}

\examples{
x <- rcauchy(60)
y <- rnorm(60)
cortest(y, x)
}



