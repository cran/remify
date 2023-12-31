% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remify.R
\name{dim.remify}
\alias{dim.remify}
\title{dim.remify}
\usage{
\method{dim}{remify}(x)
}
\arguments{
\item{x}{a \code{remify} object.}
}
\value{
vector of dimensions of the processed event sequence.
}
\description{
A function that returns the dimension of the temporal network.
}
\examples{

# processing the random network 'randomREHsmall'
library(remify)
data(randomREHsmall)
reh <- remify(edgelist = randomREHsmall$edgelist,
              model = "tie")

# dimensions of the processed 'remify' object
dim(reh)

}
