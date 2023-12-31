% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remify.R
\name{summary.remify}
\alias{summary.remify}
\title{summary.remify}
\usage{
\method{summary}{remify}(object, ...)
}
\arguments{
\item{object}{a \code{remify} object.}

\item{...}{other arguments.}
}
\value{
prints out the main characteristics of the processed relational event sequence.
}
\description{
A function that returns a easy-to-read summary of the main characteristics as to the processed relational event sequence.
}
\examples{

# processing the random network 'randomREHsmall'
library(remify)
data(randomREHsmall)
reh <- remify(edgelist = randomREHsmall$edgelist,
              model = "tie")

# printing a summary of the processed 'remify' object
summary(reh)

}
