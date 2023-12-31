% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remify.R
\name{getDyad}
\alias{getDyad}
\alias{getDyad.remify}
\title{getDyad}
\usage{
getDyad(x, dyadID, active = FALSE)

\method{getDyad}{remify}(x, dyadID, active = FALSE)
}
\arguments{
\item{x}{a \code{remify} object.}

\item{dyadID}{a vector of one or more dyad ID's, each one ranging from 1 to D (largest risk set size).}

\item{active}{logical, whether to consider the input \code{dyadID} as a vector of ID's of active dyads (\code{active = TRUE}) or dyads from the full risk set (\code{active = FALSE}).}
}
\value{
a data.frame with "actor1", "actor2" and "type" names corresponding to the vector \code{dyadID}.
}
\description{
A function that given a vector of one or more dyad ID's returns the corresponding dyad composition of "actor1", "actor2" and "type" (if event types are present). The ID's to supply must range between 1 and D (largest risk set size).
}
\section{Methods (by class)}{
\itemize{
\item \code{getDyad(remify)}: return dyad composition in actor1, actor2 and type from one (or more) dyad ID

}}
\examples{

# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# find dyad composition (names of actor1, actor2 and type) from the dyad ID
getDyad(x = reh, dyadID = c(450,239,900))

}
