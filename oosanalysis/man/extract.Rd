\name{extract.target}
\alias{extract.target}
\alias{extract.predictors}
\title{Convenience function to extract data from a model}
\description{Convenience function to extract data from a model.}
\usage{
extract.target(model, dataset)
extract.predictors(model, dataset)
}
\arguments{
  \item{model}{A function that takes a data frame as its argument and
    returns an object that has a \code{terms} method.}
  \item{dataset}{A data frame.}
}
\details{text}
\value{For \code{extract.target}, a vector that contains the values from
  \code{dataset} of the dependent variable specified in \code{model}.
  For \code{extract.predictors}, the same thing but now the
  \code{model.matrix} of the predictors.}
\references{
  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.
}
\author{Gray Calhoun <gcalhoun@iastate.edu>}

\seealso{\code{\link{mixedwindow}}, \code{\link{clarkwest}},
  \code{\link{terms}}, \code{\link{model.matrix}}}

\examples{
model <- function(d) lm(y ~ x, data = d)
dataset <- data.frame(y = rnorm(10), x = rnorm(10))

\dontshow{stopifnot(isTRUE(}
all.equal(extract.target(model, dataset), dataset$y,
          check.attributes = FALSE)
\dontshow{))}

\dontshow{stopifnot(isTRUE(}
all.equal(extract.predictors(model, dataset),
          cbind(1, dataset$x), check.attributes = FALSE)
\dontshow{))}
}

\keyword{models}