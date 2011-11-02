\name{stepm}
\alias{stepm}
\title{Romano and Wolf's (2005) StepM}
\description{Implements Romano and Wolf's StepM, a stepdown procedure
  for multiple hypothesis testing.}
\usage{
stepm(teststatistics, bootmatrix, level)
}

\arguments{
  \item{teststatistics}{A numeric vector, containing test statistics.}
  \item{bootmatrix}{A matrix with \code{length(teststatistics)} rows and
    an arbitrary number of columns.  Each column is a different
    draw from the bootstrap-induced distribution of \code{teststatistics}.}
  \item{level}{The desired confidence level of the test.}
}
\details{This function assumes that each element of
  \code{teststatistics} tests (say) \eqn{\mu_i = 0}{mu[i] = 0} against
  the alternative \eqn{mu_i > 0}{mu[i] > 0}, for \eqn{i}{i} =
  1,\dots,\code{length(teststatistics)}.  Romano and Wolf's (2005) 
  StepM procedure estimates a critical value such that the probability
  that it is smaller than at least one statistic corresponding to a true
  null hypothesis is controlled at level \code{level}.
}
\value{A list with the following elements:
  \item{criticalvalue}{The estimated critical value.}
  \item{rejected}{A logical vector indicating which statistcs exceed the
  critical value.}
}
\references{
  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.

  Romano, J. P., and Wolf, M. 2005, Stepwise multiple testing as
  formalized data snooping.  \emph{Econometrica}, \bold{73}(4), pages
  1237--1292.
}
\author{Gray Calhoun \email{gcalhoun@iastate.edu}}

\seealso{\code{\link{boot}}}
\examples{
n <- 50
nboot <- 99
d <- data.frame(x1 = rnorm(n), x2 = rnorm(n) + 1, x3 = rnorm(n))

dottests <- function(dataset)
  sapply(dataset, function(x) t.test(x)$statistic)

stepm(teststatistics = dottests(d),
      bootmatrix = replicate(nboot, dottests(d[sample(1:n, n, replace = TRUE),])),
      level = 0.05)
}

\keyword{htest}