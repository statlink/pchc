\name{Skeleton of the PC algorithm}
\alias{pchc.skel}
\title{
The skeleton of a Bayesian network learned with the PC algorithm
}

\description{
The skeleton of a Bayesian network learned with the PC algorithm.
}

\usage{
pchc.skel(x, method = "pearson", alpha = 0.05,
robust = FALSE, ini.stat = NULL, R = NULL)
}

\arguments{
\item{x}{
A numerical matrix with the variables. If you have a data.frame (i.e. categorical data) turn them into a matrix
using \code{\link{data.frame.to_matrix}}. Note, that for the categorical case data, the numbers must start from 0. No missing data are allowed.
}
\item{method}{
If you have continuous data, this "pearson". If you have categorical data though,
this must be "cat". In this case, make sure the minimum value of each variable is zero. The function "g2Test" in the R package \bold{Rfast} and the relevant functions work that way.
}
\item{alpha}{
The significance leve for assessing the p-values.
}
\item{robust}{
Do you want outliers to be removed prior to applying the PCHC algorithm? If yes, set this to TRUE to utilise the
MCD.
}
\item{ini.stat}{
If the initial test statistics (univariate associations) are available, pass them through this parameter.
}
\item{R}{
If the correlation matrix is available, pass it here.
}
}

\details{
The PC algorithm as proposed by Spirtes et al. (2000) is implemented. The variables must be either continuous or
categorical, only. The skeleton of the PC algorithm is order independent, since we are using the third heuristic
(Spirte et al., 2000, pg. 90). At every stage of the algorithm use the pairs which are least statistically associated. The conditioning set consists of variables which are most statistically associated with each other of the pair of variables.

For example, for the pair (X, Y) there can be two conditioning sets for example (Z1, Z2) and (W1, W2). All p-values and test statistics and degrees of freedom have been computed at the first step of the algorithm. Take the p-values between (Z1, Z2) and (X, Y) and between (Z1, Z2) and (X, Y). The conditioning set with the minimum p-value is used first. If the minimum p-values are the same, use the second lowest p-value. If the unlikely, but not impossible, event of all p-values being the same, the test statistic divided by the degrees of freedom is used as a means of choosing which conditioning set is to be used first.

If two or more p-values are below the machine epsilon (.Machine$double.eps which is equal to 2.220446e-16), all of them are set to 0. To make the comparison or the ordering feasible we use the logarithm of p-value. Hence,
the logarithm of the p-values is always calculated and used.

In the case of the \eqn{G^2} test of independence (for categorical data) with no permutations, we have incorporated a rule of thumb. If the number of samples is at least 5 times the number of the parameters to be estimated, the test is performed, otherwise, independence is not rejected according to Tsamardinos et al. (2006). We have modified it so that it calculates the p-value using permutations.
}

\value{
A list including:
\item{stat}{
The test statistics of the univariate associations.
}
\item{ini.pvalue}{
The initial p-values univariate associations.
}
\item{pvalue}{
The logarithm of the p-values of the univariate associations.
}
\item{runtime}{
The duration of the algorithm.
}
\item{kappa}{
The maximum value of k, the maximum cardinality of the conditioning set at which the algorithm stopped.
}
\item{n.tests}{
The number of tests conducted during each k.
}
\item{G}{
The adjancency matrix. A value of 1 in G[i, j] appears in G[j, i] also, indicating that i and j have an edge between them.
}
\item{sepset}{
A list with the separating sets for every value of k.
}
}

\references{
Spirtes P.,  Glymour C. and Scheines R. (2001). Causation, Prediction, and Search. 
The MIT Press, Cambridge, MA, USA, 3nd edition.

Tsagris M. (2021). A new scalable Bayesian network learning algorithm with applications to economics. 
Computational Economics, 57(1): 341--367.

Tsamardinos I. and Borboudakis G. (2010) Permutation Testing Improves Bayesian Network Learning. 
In Machine Learning and Knowledge Discovery in Databases. ECML PKDD 2010. 322--337.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{ \link{fedhc.skel}, \link{mmhc.skel}, \link{pchc}, \link{pchc.skel.boot} }
}

\examples{
# simulate a dataset with continuous data
x <- matrix( rnorm(300 * 30, 1, 100), nrow = 300 )
a <- pchc::pchc.skel(x)
}

