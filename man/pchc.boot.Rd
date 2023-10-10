\name{Bootstrapping the PCHC and PCTABU Bayesian network learning algorithms}
\alias{pchc.boot}
\alias{pctabu.boot}
\title{
Bootstrapping the PCHC and PCTABU Bayesian network learning algorithms
}

\description{
Bootstrapping the PCHC and PCTABU Bayesian network learning algorithms.
}

\usage{
pchc.boot(x, method = "pearson", alpha = 0.05, ini.stat = NULL,
R = NULL, restart = 10, score = "bic-g", blacklist = NULL, whitelist = NULL,
B = 200, ncores = 1)

pctabu.boot(x, method = "pearson", alpha = 0.05, ini.stat = NULL,
R = NULL, tabu = 10, score = "bic-g", blacklist = NULL, whitelist = NULL,
B = 200, ncores = 1)
}

\arguments{
\item{x}{
A numerical matrix with the variables. If you have a data.frame (i.e. categorical data) turn them into a matrix
using \code{\link{data.frame.to_matrix}}. Note, that for the categorical case data, the numbers must start from 0.
No missing data are allowed.
}
\item{method}{
If you have continuous data, you can choose either "pearson" or "spearman". If you have categorical data though,
this must be "cat". In this case, make sure the minimum value of each variable is zero. The \code{\link{g2test}}
and the relevant functions work that way.
}
\item{alpha}{
The significance level for assessing the p-values.
}
\item{ini.stat}{
If the initial test statistics (univariate associations) are available, pass them through this parameter.
}
\item{R}{
If the correlation matrix is available, pass it here.
}
\item{restart}{
An integer, the number of random restarts.
}
\item{tabu}{
An integer, the length of the tabu list used in the tabu function.
}
\item{score}{
A character string, the label of the network score to be used in the algorithm. If none is specified,
the default score is the Bayesian Information Criterion for both discrete and continuous data sets.
The available score for continuous variables are: "bic-g" (default), "loglik-g", "aic-g", "bic-g" or "bge".
The available score categorical variables are: "bde", "loglik" or "bic".
}
\item{blacklist}{
A data frame with two columns (optionally labeled "from" and "to"), containing a set of arcs not to
be included in the graph.
}
\item{whitelist}{
A data frame with two columns (optionally labeled "from" and "to"), containing a set of arcs to be
included in the graph.
}
\item{B}{
The number of bootstrap resamples to draw. The algorithm is performed in each bootstrap sample. In the end, the adjacency matrix on the observed data is returned, along with another adjacency matrix produced by the bootstrap. The latter one contains values from 0 to 1 indicating the proportion of times an edge between two nodes was present.
}
\item{ncores}{
The number of cores to use, in case of parallel computing.
}
}

\details{
The PC algorithm as proposed by Spirtes et al. (2001) is first implemented followed by a scoring phase,
such as hill climbing or tabu search. The PCHC was proposed by Tsagris (2021), while the PCTABU algorithm
is the same but instead of the hill climbing scoring phase, the tabu search is employed.
}

\value{
A list including:
\item{mod}{
A list including the output of the \code{ \link{pchc} } or the \code{ \link{pctabu} }  function.
}
\item{Gboot}{
The bootstrapped adjancency matrix of the Bayesian network.
}
\item{runtime}{
The duration of the algorithm.
}
}

\references{
Tsagris M. (2021). A new scalable Bayesian network learning algorithm with applications to economics.
Computational Economics, 57(1): 341--367.

Spirtes P.,  Glymour C. and Scheines R. (2001). Causation, Prediction, and Search. The MIT Press, Cambridge, MA, USA, 3nd edition.

Tsamardinos I. and Borboudakis G. (2010) Permutation Testing Improves Bayesian Network Learning. 
In Machine Learning and Knowledge Discovery in Databases. ECML PKDD 2010, 322--337.

Tsamardinos I., Brown E.L. and Aliferis F.C. (2006). The max-min hill-climbing Bayesian network
structure learning algorithm. Machine Learning, 65(1): 31--78.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}
\seealso{
\code{\link{fedhc}, \link{mmhc}, \link{pchc}, \link{pchc.skel} }
}

\examples{
# simulate a dataset with continuous data
x <- matrix( rnorm(200 * 20, 1, 10), nrow = 200 )
a <- pchc.boot(x, B = 50)
}

