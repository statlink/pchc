\name{Correlation matrix for FBM class matrices (big matrices)}
\alias{big_cor}
\title{
Correlation matrix for FBM class matrices (big matrices)
}

\description{
Correlation matrix for FBM class matrices (big matrices).
}

\usage{
big_cor(x)
}

\arguments{
\item{x}{
An FBM class matrix.
}
}

\details{
The function accepts a Filebacked Big Matrix (FBM) class matrix and returns the correlation matrix.
Check you matrix for possible NA values. For more information see the "bigmemory" and "bigstatsr" packages.
}

\value{
The correlation matrix of the big data x.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{big_read}, \link{fedhc.skel}, \link{mmhc.skel} }
}

\examples{
\donttest{
require(bigstatsr, quietly = TRUE)
x <- matrix( runif(100 * 50, 1, 100), ncol = 50 )
x <- bigstatsr::as_FBM(x)
a <- pchc::big_cor(x)
}
}

