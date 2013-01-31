\name{rvar}
\alias{rvar}
\title{Generate pseudo-random data from a Vector Autoregression}
\newcommand{\VAR}{\acronym{VAR}}
\description{This function generates data from a user-specified Vector
  Autoregression (\VAR)}
\usage{
rvar(nobs, coefficients, intercept, vcv, nburn = 1000,
     y0 = matrix(0, nlag, neq))
}
\arguments{
  \item{nobs}{Integer; the number of observations to generate.}
  \item{coefficients}{A list of numeric vectors; each vector specifies
    the coefficients of one of the \VAR equations.}
  \item{intercept}{A numeric vector containing the intercepts.}
  \item{vcv}{A numeric matrix representing the variance-covariance
    matrix of the innovations.}
  \item{nburn}{An integer, the number of draws to discard before
    generating draws from the \VAR.}
  \item{y0}{A matrix containing the inital values of the series.}
}
\details{Let \eqn{\beta_1} represent the first element of the
  coefficients list and suppose that there are \eqn{k} equations with
  \eqn{l} lags.  The first equation of the \acronym{DGP} is
  \deqn{y_{1t} = \mu_1 + (y_{1,t-1},\dots,y_{1,t-l}, y_{2,t-1}, \dots,
    y_{2,t-l},\dots y_{k,t-1},\dots, y_{k,t-l})' \beta_1 +
    \varepsilon_{1t}}{y[t,1] = intercept[1] +
    c(y[t-(1:l),1],\dots,y[t-(1:l),k]) \%*\% coefficients[[1]] + epsilon[t,1]}.
}
\value{A data frame with nobs rows containing the generated series and
  all of their lags.  This is redundant information, but makes it easier
  to use these results directly in a regression model.}
\author{Gray Calhoun \email{gcalhoun@iastate.edu}}

\examples{
d <- rvar(10000, list(a = c(0.5, 0, 0.2, 0.1),
                      b = c(0.1, 0.2, 0.5, 0)),
          c(4, 6), diag(2))

lm(a ~ aL1 + aL2 + bL1 + bL2, data = d)
lm(b ~ aL1 + aL2 + bL1 + bL2, data = d)
}

\keyword{datagen}
\keyword{ts}
