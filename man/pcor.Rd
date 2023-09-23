\name{Partial correlation between two continuous variables}
\alias{pcor}
\title{
Partial correlation
}

\description{
Partial correlation between two continuous variables when a correlation matrix is given.
}

\usage{
pcor(R, indx, indy, indz, n)
}

\arguments{
\item{R}{
A correlation or covariance matrix.
}
\item{indx}{
The index of the first variable whose conditional correlation is to estimated.
}
\item{indy}{
The index of the second variable whose conditional correlation is to estimated.
}
\item{indz}{
The index of the conditioning variables.
}
\item{n}{
The sample size of the data from which the correlation matrix was computed.
}
}

\details{
Given a correlation or a covariance matrix the function will caclulate the partial correlation between variables indx and indy conditioning on variable(s) indz and will return the logarithm of the p-value.
}

\value{
A numeric vector containing the partial correlation and logged p-value for the test of no partial correlation.
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
\code{ \link{cor2pcor}, \link{cortest}, \link{correls}
}
}

\examples{
y <- as.matrix( iris[, 1:2] )
z <- cbind(1, iris[, 3] )
er <- resid( .lm.fit(z, y) )
r <- cor(er)[1, 2]
z <- 0.5 * log( (1 + r) / (1 - r) ) * sqrt( 150 - 1 - 3 )
log(2) + pt( abs(z), 150 - 1 - 3, lower.tail = FALSE, log.p = TRUE )
r <- cor(iris[, 1:3])
pcor(r, 1,2, 3, 150)
}

