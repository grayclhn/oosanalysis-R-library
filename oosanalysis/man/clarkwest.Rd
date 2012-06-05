\name{clarkwest}
\alias{clarkwest}
\alias{clarkwest_calculation}
\title{Clark and West's (2006, 2007) Out-of-Sample Test}
\description{Functions to calculate Clark and West's (2006, 2007)
  approximately normal \acronym{oos} statistic.}
\usage{
clarkwest(null, alt, dataset, R, vcv = var,
          window = c("rolling", "recursive", "fixed"))

clarkwest_calculation(target, null.forecast, alt.forecast, vcv)
}
\arguments{
  \item{null}{A function that takes a subset of the data \code{dataset}
    as its argument and returns an object with a \code{predict} method.
    This function generates the benchmark forecast.}
  \item{alt}{A second function that takes a subset of the data \code{dataset}
    as its argument and returns an object with a \code{predict} method.
    This function generates the alternative forecast.}
  \item{dataset}{A data frame.}
  \item{R}{An integer, the size of the training sample.}
  \item{vcv}{A function to calculate the asymptotic variance of the
    \acronym{oos} average.}
  \item{window}{A character that indicates the window strategy for
    \acronym{oos} estimation.}
  \item{target}{A vector containing the values of the predictand.}
  \item{null.forecast}{A vector containing the values of the benchmark
    forecast.} 
  \item{alt.forecast}{A vector containing the values of the alternative
    forecast.}
}

\value{Both functions return the same thing, a list with elements
  \item{mu}{an estimate of the corrected \acronym{oos} mean,}
  \item{avar}{the asymptotic variance of the corrected \acronym{oos}
    average,}
  \item{pvalue}{the p-value associate with the one-sided \acronym{oos}
  test.}
}

\details{Both of these functions implement Clark and West's (2006, 2007)
  "corrected" out-of-sample tests.  The idea behind their tests is that
  using a fixed-length rolling window, as in Giacomini and White (2006),
  ensures that the \acronym{oos} average is asymptotically normal.  In
  Giacomini and White, though, the \acronym{oos} average is not centered
  at the expected difference in the \acronym{mse} of the pseudo-true
  forecasting models, so Clark and West introduce an adjustment so that
  their statistic is centered correctly.  Be aware that Clark and West's
  adjustment is provably correct for the fixed or rolling windows when
  \code{R} is small \emph{and the benchmark model is not estimated},
  though Clark and West's (2007) simulations indicate that it performs
  well for estimated benchmarks for some \acronym{dgp}s.  See Calhoun
  (2011) for an asymptotically normal \acronym{oos} statistic that
  allows the benchmark to be estimated.  The function allows users to
  choose the "recursive" estimation strategy because it is popular in
  practice, but be careful.

  \code{clarkwest_calculation} does all of the algebra and
  \code{clarkwest} is a convenient interface to it that calculates the
  forecasts automatically.
}

\references{

  Calhoun, G. 2011, An asymptotically normal out-of-sample test of equal
  predictive accuracy for nested models.  Unpublished manuscript.

  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.

  Clark, T. E., West, K. D. 2006, Using out-of-sample mean squared
  prediction errors to test the martingale difference hypothesis.
  \emph{Journal of Econometrics}, \bold{135}(1): 155--186.

  Clark, T. E., West, K. D. 2007, Approximately normal tests for equal
  predictive accuracy in nested models.  \emph{Journal of Econometrics},
  \bold{138}(1): 291--311.

}

\author{Gray Calhoun \email{galhoun@iastate.edu}}

\seealso{\code{\link{dmw_calculation}}, \code{\link{mixedwindow}},
  \code{\link{mccracken_criticalvalue}},
  \code{\link{recursive_forecasts}}, \code{\link{predict}}}

\examples{
x <- rnorm(100)
d <- data.frame(y = x + rnorm(100), x = x)
R <- 70

model1 <- function(d) lm(y ~ 1, data = d)
model2 <- function(d) lm(y ~ x, data = d)

clarkwest(model1, model2, d, R, window = "rolling")
}

\keyword{ts}
\keyword{htest}
\keyword{models}
