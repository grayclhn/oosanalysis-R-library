\name{mccracken_criticalvalue}
\alias{mccracken_criticalvalue}
\title{Returns McCracken's (2007) oos-t critical values}
\description{This function retrieves the critical values for the oos-t
  test statistic derived by McCracken (2007) for nested models.}
\usage{
mccracken_criticalvalue(pi, k2, size, window = c("recursive", "rolling", "fixed"))
}
\arguments{
  \item{pi}{P/R where P is the number of out-of-sample observations and
    R is the size of the estimation window.  Note that this is rounded
    to the closest value in McCracken's table; if you want to
    interpolate between listed values, you must do so manually.}
  \item{k2}{The number of additional regressors in the larger model.}
  \item{size}{The asymptotic nominal size of the test; this must be
    0.90, 0.95, or 0.99.}
  \item{window}{The OOS window scheme.}
}
\value{Returns a single numeric value, the appropriate critical value
  for the test.}
\references{
  McCracken, M. 2007, Asymptotics for out of sample tests of Granger
  causality.  \emph{Journal of Econometrics}, \bold{140}(2): 719-752.
}
\author{Gray Calhoun \email{gcalhoun@iastate.edu}}

\seealso{\code{\link{clarkwest}}, \code{\link{recursive_forecasts}},
  \code{\link{dmw_mse}}, \code{\link{dmw_calculation}}}

\examples{
mccracken_criticalvalue(.4, 5, .9, "rolling")
mccracken_criticalvalue(.4, 5, .9, "recursive")
mccracken_criticalvalue(.4, 5, .9, "fixed")
}
\keyword{ts}
\keyword{htest}
