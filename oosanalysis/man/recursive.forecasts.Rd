\name{recursive.forecasts}
\alias{recursive.forecasts}
\title{Pseudo out-of-sample forecasts}
\description{Creates a sequence of out-of-sample forecasts}
\usage{
recursive.forecasts(model, dataset, R,
                    window = c("recursive", "rolling", "fixed"),...)
}
\arguments{
  \item{model}{A function that takes \code{dataset} as its first argument.  \code{model}
    must return an object with a \code{predict} method.}
  \item{dataset}{A data frame with more than \code{R} observations}
  \item{R}{An integer: the size of the estimation window.}
  \item{window}{One of "rolling", "recursive", or "fixed" describing the
  estimation strategy}
\item{\dots}{Additional arguments to pass to \code{model}}
}
\details{
  Uses \code{model} to create a sequence of forecasts or forecast errors for
  observations \code{R+1},\dots,\code{nrow(dataset)}.
  For the "rolling" window, each forecast comes from the model estimated
  with the previous \code{R} observations.  For the "recursive" window, each
  forecast uses all of the previous observations.  And for the "fixed"
  window, each forecast uses the first \code{R} observations.
}
\value{A vector of length \code{nrow(dataset) - R}, containing the forecasts.}

\references{
  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.
}

\author{Gray Calhoun <gcalhoun@iastate.edu>}

\examples{
d <- data.frame(x = rnorm(15), y = rnorm(15))
ols <- function(d) lm(y ~ x, data = d)
## Basic Usage:
recursive.forecasts(ols, d, 4, "recursive")

## Illustrate different estimation windows by comparing forecasts for
## observation 11 (note that the forecast for observation 11 will be the
## 7th element that apply.oos returns in this example)
newd <- d[11,]
check <- function(a, b) stopifnot(all.equal(a, b, check.attributes = FALSE))

check(predict(lm(y ~ x, data = d[7:10,]), newd),
      recursive.forecasts(ols, d, 4, "rolling")[7])

check(predict(lm(y ~ x, data = d[1:10,]), newd),
      recursive.forecasts(ols, d, 4, "recursive")[7])

check(predict(lm(y ~ x, data = d[1:4,]), newd),
      recursive.forecasts(ols, d, 4, "fixed")[7])
}

\keyword{ts}
\keyword{models}
