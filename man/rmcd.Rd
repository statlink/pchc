\name{Outliers free data via the reweighted MCD}
\alias{rmcd}
\title{
Outliers free data via the reweighted MCD
}

\description{
Outliers free data via the reweighted MCD.
}

\usage{
rmcd(x, alpha = NULL)
}

\arguments{
\item{x}{
A numerical matrix with the variables. If you have a data.frame (i.e. categorical data) turn them into a matrix.
}
\item{alpha}{
A number controlling the size of the subsets over which the determinant is minimized; roughly alpha*n observations are used for computing the determinant. Values between 0.5 and 1 are allowed.
}
}

\details{
The FEDHC algorithm.
}

\value{
A list including:
\item{poia}{
A vector with the indices of the vectors that were removed.
}
\item{x}{
The outlier free data.
}
}

\references{
Rousseeuw P. J. and Leroy A. M. (1987) Robust Regression and Outlier Detection. Wiley.

Rousseeuw P. J. and van Driessen K. (1999) A fast algorithm for the minimum covariance determinant estimator. Technometrics, 41: 212--223.

Pison G., Van Aelst S., and Willems G. (2002) Small Sample Corrections for LTS and MCD. Metrika, 55: 111--123.

Hubert M., Rousseeuw P. J. and Verdonck, T. (2012) A deterministic algorithm for robust location and scatter. Journal of Computational and Graphical Statistics, 21: 618--637.

Cerioli A. (2010). Multivariate outlier detection with high-breakdown estimators. Journal of the American Statistical Association, 105(489): 147--156.

Cerchiello P. and Giudici P. (2016). Big data analysis for financial risk management. Journal of Big Data, 3(1): 18.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{fedhc.skel}, \link{pchc.skel}, \link{mmhc.skel} }
}

\examples{
x <- matrix( rnorm(200 * 20), nrow = 200 )
x1 <- matrix( rnorm(10 * 20, 10), nrow = 10 )
x <- rbind(x, x1)
a <- pchc::rmcd(x)
a$poia
}
