\name{ForecastOOS}
\alias{ForecastOOS}
\alias{OlsFixed}
\alias{OlsRolling}
\alias{OlsRecursive}
\title{
Construct Pseudo Out-of-Sample Forecasts
}
\description{Generates a matrix of pseudo out-of-sample forecasts using
  different training samples.
}
\usage{
ForecastOOS(formula, model, data, R, scheme, \dots)
OlsFixed(X, Y, R)
OlsRolling(X, Y, R)
OlsRecursive(X, Y, R)
}
\arguments{
  \item{formula}{An object of class 'formula' to be passed to the
    forecasting model.}
  \item{model}{A function that takes the arguments 'formula' and 'data'
    and returns an object that has a 'predict' method.  This function
    will be used to construct the forecasts.}
  \item{data}{An object of class 'data.frame' containing the variables
    that will be used for forecasting.}
  \item{R}{An integer vector that specifies the lengths of each
    estimation sample.}
  \item{scheme}{Either "fixed", "recursive", or "rolling" -- specifies
    the estimation strategy that will be used for the forecasts.}
  \item{\dots}{Additional arguments to pass to \code{model}.}

}

\value{A matrix of dimension \code{dim(data)[1] - min(R)} by
  \code{length(R)}.  Column \code{i} contains the forecasts generated
  using an estimation window of length \code{R[i]}.  The argument
  'scheme' dictates how these forecasts are calculated.  For 'fixed,'
  the model is estimated once for each column, using all of the data through
  \code{R[i]}.  For 'recursive,' the model is reestimated for each
  forecast using all of the preceeding data.  For 'rolling,' the model
  is reestimated for each forecast using only the \code{R[i]} most
  recent observations.
}

\details{
  \code{ForecastOOS} is basically a convenience wrapper for using
  \code{lapply} or \code{sapply} to call \code{predict(model(\dots),
    \dots)} repeatedly.  It is not terribly efficient.

}

\examples{
d <- data.frame(Y = rnorm(100), X1 = rnorm(100), X2 = rnorm(100))
R <- c(80, 90, 95)
ForecastOOS(Y ~ X1 + X2, lm, d, R, "fixed")
ForecastOOS(Y ~ X1 + X2, lm, d, R, "rolling")
ForecastOOS(Y ~ X1 + X2, lm, d, R, "recursive")

## Check that the forecasts line up with direct forecasts
## from lm under different schemes.

TestThis(all.equal(
  drop(ForecastOOS(Y ~ X1 + X2, lm, d, 80, "fixed")),
  predict(lm(Y ~ X1 + X2, d, subset = 1:80), d[81:100, ])
))

TestThis(all.equal(
  drop(ForecastOOS(Y ~ X1 + X2, lm, d, 80, "recursive")["91", ]),
  unname(predict(lm(Y ~ X1 + X2, d, subset = 1:90), d[91, ]))
))

TestThis(all.equal(
  drop(ForecastOOS(Y ~ X1 + X2, lm, d, 80, "rolling")["91", ]),
  unname(predict(lm(Y ~ X1 + X2, d,
                    subset = seq(to = 90, length = 80)), d[91, ]))
))

}
\keyword{models}
