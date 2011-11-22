\name{Classes}
\alias{Aggregate}
\alias{CT}
\alias{predict.CT}
\alias{predict.Aggregate}
\alias{HasMethod}
\title{Some classes to simplify making predictions}
\description{This file defines some classes and methods that are used to
  make some of the predictions in the corresponding paper's empirical
  exercise (Calhoun, 2011).
}
\usage{
CT(model)
Aggregate(model.list, fn)
\method{predict}{CT}(object, newdata,\dots)
\method{predict}{Aggregate}(object, newdata,\dots)

HasMethod(object, method.name)
}
\arguments{
  \item{model}{An object with a \dQuote{predict} method.}
  \item{model.list}{A list of objects with \dQuote{predict} methods.}
  \item{fn}{A function that can take and aggregate a vector:
    \dQuote{mean,} for example.}
  \item{object}{A variable belonging to an S3 class.}
  \item{newdata}{A new data set to use to create the new forecasts.}
  \item{\dots}{For \dQuote{predict.CT}, additional arguments to pass to
    the underlying \dQuote{predict} method.  For
    \dQuote{predict.Aggregate}, additional arguments to pass to the
    individual \dQuote{predict} methods.}
  \item{method.name}{A character vector giving the names of different S3
  methods.}
}
\value{\dQuote{CT} and \dQuote{Aggregate} return objects with (S3) classes
  \dQuote{CT} and \dQuote{Aggregate} respectively.  The \dQuote{predict}
  methods each return a single forecast.  \dQuote{HasMethod} returns a
  logical vector with the same length as \dQuote{method.name} indicating
  whether \dQuote{object} has each method defined.
}
\details{
}

\examples{
  olddata <- data.frame(y = rnorm(30), x = rnorm(30))
  newdata <- data.frame(y = rnorm(3), x = rnorm(3))

  m1 <- lm(y ~ 1, data = olddata)
  m2 <- lm(y ~ x, data = olddata)

  m3 <- CT(m2)
  m4 <- Aggregate(list(m1, m2, m3), median)

  predict(m3, newdata)
  predict(m4, newdata)

  HasMethod(m1, c("plot", "print", "predict", "median"))
}
\author{Gray Calhoun \email{gcalhoun@iastate.edu}}
\keyword{classes}
\keyword{models}
