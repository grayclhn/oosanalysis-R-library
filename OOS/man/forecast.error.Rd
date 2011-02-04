\name{forecast.error}
\alias{forecast.error}
\title{Forecast Errors}
\description{'forecast.error' uses the 'predict' method to calculate a
  model's forecast error on a new dataset.}
\usage{forecast.error(object, newdata, ...)}

\arguments{
  \item{object}{a model object that has a 'predict' method.}
  \item{newdata}{a data.frame to use for new predictions.}
  \item{\dots}{additional arguments to pass to 'predict'.}}

\details{This function is designed to automatically get the forecast
  errors associated with a prediction.  It is based on predict.lm, so
  errors may come up when it is used for other models.}
\value{A vector of length nobs(newdata), containing the prediction errors.}
\author{Gray Calhoun <gcalhoun@iastate.edu>}

\seealso{\code{\link{predict}}, \code{\link{nobs-methods}}}
\examples{

d    <- data.frame(y = rnorm(10), x = rnorm(10))
newd <- data.frame(y = rnorm(10), x = rnorm(10))

mod  <- lm(y ~ x, data = d)

forecast.error(mod, newd)

# Calculate forecast errors by hand:
errs <- newd$y - predict(mod, newd)

stopifnot(all.equal(errs, forecast.error(mod, newd)))
}