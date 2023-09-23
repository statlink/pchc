\name{Read big data or a big.matrix object}
\alias{big_read}
\title{
Read big data or a big.matrix object
}

\description{
Read big data or a big.matrix object.
}

\usage{
big_read(big_path, select, header = TRUE, sep = ",")
}

\arguments{
\item{big_path}{
The path (including the name) where the big.matrix object is.
}
\item{select}{
Indices of columns to read (sorted). The length of select will be the number of columns of the
resulting filebacked Big Matrix.
}
\item{header}{
If there are column names, then this should be TRUE.
}
\item{sep}{
A field delimiter, for example ";" or "," (comma separated). See also \code{\link{read.csv}} for more information.
}
}

\details{
The data (matrix) which will be read and compressed into a big.matrix object must be of type "numeric".
I tested it and it works with "integer" as well. But, in general, bear in mind that only matrices will be read.
I have not tested with data.frame for example. However, in the help page of "bigmemory" this is mentioned:
Any non-numeric entry will be ignored and replaced with NA, so reading something that traditionally would
be a data.frame won't cause an error. A warning is issued. In all cases, the big.matrix is turned into a
Filebacked Big Matrix (FBM) of type 'double' the object size is alwasy 680 bytes! If the initial dataset
has row names these will be ignored and a column with NAs will apear. So check your final FBM matrix.
For more information see the "bigmemory" and "bigstatsr" packages.
}

\value{
A Filebacked Big Matrix (FBM) matrix.
}

\author{
Michail Tsagris.

R implementation and documentation: Michail Tsagris \email{mtsagris@uoc.gr}.
}

\seealso{
\code{ \link{big_cor}, \link{fedhc.skel}, \link{mmhc.skel} }
}

\examples{
x <- matrix( runif(100 * 5, 1, 100), ncol = 5 )
}

