\name{stepm}
\alias{stepm}
\title{Romano and Wolf's (2005) StepM}
\description{Implements Romano and Wolf's StepM, a stepdown procedure
  for multiple hypothesis testing.}
\usage{
stepm(teststatistics, bootmatrix, lefttail, righttail)
}

\arguments{
  \item{teststatistics}{A numeric vector, containing test statistics.}
  \item{bootmatrix}{A matrix with \code{length(teststatistics)} rows and
    an arbitrary number of columns.  Each column is a different
    draw from the bootstrap-induced distribution of \code{teststatistics}.}
  \item{lefttail}{The mass to be left in the left tail of the
  distribution.  Setting this to \code{NA} imposes a one-sided alternative.}
  \item{righttail}{The mass to be left in the right tail of the
  distribution.  Setting this to \code{NA} imposes a one-sided
  alternative.}
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
  \item{criticalvalues}{The estimated critical values.}
  \item{rejected}{A logical vector indicating which statistcs fall outside the
  critical values.}
}
\references{
  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.

  Calhoun, G. 2012, A comment on "Stepwise multiple testing as
  formalized data snooping."  Unpublished manuscript.

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
      lefttail = NA, righttail = 0.05)
}

\keyword{htest}