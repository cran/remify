% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/remify.R
\name{getActorID}
\alias{getActorID}
\alias{getActorID.remify}
\title{getActorID}
\usage{
getActorID(x, actorName = NULL)

\method{getActorID}{remify}(x, actorName = NULL)
}
\arguments{
\item{x}{a \code{remify} object.}

\item{actorName}{a vector of actor names. The same names in the input edgelist.}
}
\value{
actor ID as integer value.
}
\description{
A function that given a vector of actor names returns the corresponding vector of ID's.
}
\section{Methods (by class)}{
\itemize{
\item \code{getActorID(remify)}: return actor's ID from actor's name

}}
\examples{

# processing the random network 'randomREH'
library(remify)
data(randomREH)
reh <- remify(edgelist = randomREH$edgelist,
              model = "tie",
              riskset = "manual",
              omit_dyad = randomREH$omit_dyad)

# find actor ID from the actor name
getActorID(x = reh, actorName = c("Francesca","Kayla"))

}
