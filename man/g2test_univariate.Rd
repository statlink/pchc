\name{All pairwise G-square and chi-square tests of indepedence}
\alias{g2test_univariate}
\alias{g2test_univariate_perm}
\alias{chi2test_univariate}

\title{
All pairwise G-square and chi-square tests of indepedence
}

\description{
All pairwise G-square and chi-square tests of indepedence.
}

\usage{
g2test_univariate(x, dc)
g2test_univariate_perm(x, dc, B)
chi2test_univariate(x, dc)
}

\arguments{
\item{x}{
A numerical matrix with the data. \bold{The minimum must be 0, otherwise the function can crash or will produce
wrong results}. The data must be consecutive numbers.
}
\item{dc}{
A numerical value equal to the number of variables (or columns of the data matrix) indicating the number of distinct, unique values (or levels) of each variable.
Make sure you give the correct numbers here, otherwise the degrees of freedom will be wrong.
}
\item{B}{
The number of permutations. The permutations test is slower than without permutations and should be used with small sample sizes or when the contigency tables have zeros. When there are few variables, R's "chisq.test" function is faster, but as the number of variables increase the time difference
with R's procedure becomes larger and larger.
}
}

\details{
The function does all the pairwise \eqn{G^2} test of independence and gives the position inside the matrix.
The user must build the associations matrix now, similarly to the correlation matrix. See the examples of how to do that.
The p-value is not returned, we live this to the user. See the examples of how to obtain it.
}

\value{
A list including:
\item{statistic}{
The \eqn{G^2} or \eqn{X^2} test statistic for each pair of variables.
}
\item{pvalue}{
The p-value of the test statistic for each pair of variables.
}
\item{x}{
The row or variable of the data.
}
\item{y}{
The column or variable of the data.
}
\item{df}{
The degrees of freedom of each test.
}
}

\references{
Tsagris M. (2017). Conditional independence test for categorical data using Poisson log-linear model.
Journal of Data Science, 15(2): 347--356.

Tsamardinos I. and Borboudakis G. (2010). Permutation testing improves Bayesian network learning.
In Joint European Conference on Machine Learning and Knowledge Discovery in Databases (pp. 322--337). 
Springer Berlin Heidelberg.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{\link{g2test}, \link{cat.tests}
}
}

\examples{
nvalues <- 3
nvars <- 10
nsamples <- 1000
x<- matrix( sample( 0:(nvalues - 1), nvars * nsamples, replace = TRUE ), nsamples, nvars )
dc <- rep(nvalues, nvars)
system.time( g2test_univariate(x, dc) )
a <- g2test_univariate(x, dc)
}




