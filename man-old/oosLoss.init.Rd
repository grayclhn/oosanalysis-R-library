\name{oosLoss.init}
\alias{oosLoss.init}
\title{
Initialize oosLoss Object
}
\description{Initializes a new oosLoss object for normal regressors and
  errors and MSE loss.
}
\usage{oosLoss.init(epSecond = 1)}
\arguments{
  \item{epSecond}{The variance of the error term}
}

\value{Returns an oosLoss object.}

\seealso{\code{"\linkS4class{oosLoss}"}}
\examples{
print(oosLoss.init())

## Check numerically that RData and LossFn are consistent with
## ELoss.  This is pretty ad hoc.

set.seed(207)
nobs <- 1000000
nreg <-      10
oosObject <- oosLoss.init()
estimates <- rnorm(nreg)

d <- r(oosObject, nobs, nreg)
pred.errors <- L(oosObject, d$Y - crossprod(t(d$X), estimates))

TestThis(abs(mean(pred.errors) - E(oosObject, estimates)) > 0.0001)
}

