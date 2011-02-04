\name{apply.oos}
\alias{apply.oos}
\title{Out-of-sample forecast errors
}
\description{Creates a sequence of out-of-sample forecasts or forecast errors
}
\usage{
apply.oos(R, d, model, window = c("rolling", "recursive", "fixed"), ret = c("forecast", "error"), ...)
}
\arguments{
  \item{R}{An integer: the size of the estimation window.}
  \item{d}{A data.frame with more than R observations}
  \item{model}{A function that takes "d" as its first argument.  'model'
    must return an object with a 'predict' method and a 'forecast.error'
    method ('forecast.error' is only used if ret = "error").}
  \item{window}{One of "rolling", "recursive", or "fixed" describing the
  estimation strategy}
  \item{ret}{One of "forecast" or "error", indicating whether to return
    the model's forecast or its forecast error.}
  \item{\dots}{Additional arguments to pass to 'model'}}
\details{
  Uses 'model' to create a sequence of forecasts or forecast errors for
  observations R+1,\dots,nobs(d).
  For the "rolling" window, each forecast comes from the model estimated
  with the previous R observations.  For the "recursive" window, each
  forecast uses all of the previous observations.  And for the "fixed"
  window, each forecast uses the first R observations.
}
\value{A vector of length nobs(d) - R, containing the forecasts or
  forecast errors.}
\author{Gray Calhoun <gcalhoun@iastate.edu>}

\seealso{\code{\link{nobs}}, \code{\link{forecast.error}}}
\examples{
d <- data.frame(x = rnorm(15), y = rnorm(15))
ols <- function(d) lm(y ~ x, data = d)
## Basic Usage:
apply.oos(4, d, ols, "recursive", "forecast")

## Illustrate different estimation windows by comparing forecasts for
## observation 11 (note that the forecast for observation 11 will be the
## 7th element that apply.oos returns in this example)
newd <- d[11,]
check <- function(a, b) stopifnot(all.equal(a, b))

check(predict(lm(y ~ x, data = d[7:10,]), newd),
      apply.oos(4, d, ols, "rolling", "forecast")[7])

check(predict(lm(y ~ x, data = d[1:10,]), newd),
      apply.oos(4, d, ols, "recursive", "forecast")[7])

check(predict(lm(y ~ x, data = d[1:4,]), newd),
      apply.oos(4, d, ols, "fixed", "forecast")[7])

## Same thing for forecast errors:

check(newd$y - predict(lm(y ~ x, data = d[7:10,]), newd),
      apply.oos(4, d, ols, "rolling", "error")[7])

check(newd$y - predict(lm(y ~ x, data = d[1:10,]), newd),
      apply.oos(4, d, ols, "recursive", "error")[7])

check(newd$y - predict(lm(y ~ x, data = d[1:4,]), newd),
      apply.oos(4, d, ols, "fixed", "error")[7])
}

\keyword{ts}
\keyword{models}
