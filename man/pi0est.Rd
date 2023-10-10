\name{Estimation of the percentage of null p-values}
\alias{pi0est}
\title{
Estimation of the percentage of null p-values
}

\description{
Estimation of the percentage of null p-values.
}

\usage{
pi0est(p, lambda = seq(0.05, 0.95, by = 0.01), dof = 3)
}

\arguments{
\item{p}{
A vector of p-values.
}
\item{lambda}{
A vector of values of the tuning parameter lambda.
}
\item{dof}{
Number of degrees of freedom to use when estimating pi_0 with smoothing splines.
}
}

\details{
The estimated proporiton of null p-values is estimated the algorithm by Storey and Tibshirani (2003).
}

\value{
The estimated proportion of non significant (null) p-values. In the paper Storey and Tibshirani mention that the estimate of pi0 is with lambda=1, 
but in their R code they use the highest value of lambda and thus we do the same here.
}
\references{
Storey J.D. and Tibshirani R. (2003). Statistical significance for genome-wide experiments. Proceedings of the National Academy of Sciences, 100: 9440--9445.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
  %%  ~~further notes~~
    %}
\seealso{
  \code{\link{conf.edge.lower}, \link{bn.skel.utils}, \link{mmhc.skel} }
}

\examples{
A <- pchc::rbn2(1000, p = 20, nei = 3)
x <- A$x
mod <- pchc::mmhc.skel(x, alpha = 0.05 )
pval <- exp(mod$pvalue)
pval <- lower.tri(pval)
pchc::pi0est(pval)
}
