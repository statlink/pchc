\name{Plot of a Bayesian network}
\alias{bnplot}

\title{
Plot of a Bayesian network
}

\description{Plot of a Bayesian network.
}

\usage{
bnplot(dag, shape = "ellipse", main = NULL, sub = NULL, highlight = NULL)
}

\arguments{
\item{dag}{
A BN object, an object of class "bn".
}
\item{shape}{
A character string defining the shape of the nodes, "ellipse" (default value), "circle" or "rectangle".
}
\item{main}{
The main title of the graph displayed on the top.
}
\item{sub}{
The subtitle of the graph displayed at the bottom.
}
\item{highlight}{
A list with options specifying which nodes to plot with different colours. You can also check the package bnlearn or
the package Rgraphviz for more information on this, or simply check the example below.
}
}

\details{
The function is called from the "bnlearn" package which invokes the "Rgraphviz" package from Bioconductor and you need to install it first.
}

\value{
The Bayesian network is visualised.
}

%\references{
%}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

%\note{
%%  ~~further notes~~
%}

\seealso{
\code{ \link{pchc}, \link{pc.skel}
}
}

\examples{
\donttest{
if (require("Rgraphviz") ) {
# simulate a dataset with continuous data
x <- matrix( rnorm(100 * 15, 1, 5), nrow = 100 )
colnames(x) <- paste("X", 1:15, sep = "")
nam <- colnames(x)
a <- pchc(x)
bnplot(a$dag)
bnplot( a$dag, highlight = list(nodes = nam[c(2, 3)],
col = "tomato", fill = "orange") )
}
}
}

