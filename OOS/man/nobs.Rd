\name{nobs}
\alias{nobs}
\title{A generic function that gets the number of observations of
  an object}

\description{A generic function that gets the number of observations of
  an object}

\usage{nobs(x, ...)}

\arguments{
  \item{x}{An object that we want to get the number of observations of.}
  \item{\dots}{Not yet implemented.}
}

\value{A single number, the number of observations in 'x'.}
\author{Gray Calhoun <gcalhoun@iastate.edu>}

\seealso{\code{\link{nobs-methods}}}

\examples{
d <- data.frame(x = 1:10, y = 1:10)
nobs(d)
}

