\name{mixedwindow}
\alias{mixedwindow}
\alias{mixedbootstrap}
\title{Calhoun's (2011) Out-of-Sample comparison}
\description{Implements the asymptotically normal OOS test statistics
  proposed by Calhoun (2011).}
\usage{
mixedwindow(null, alt, dataset, R, vcv = var,
            window = c("rolling", "fixed"))

mixedbootstrap(null, alt.list, dataset, R, nboot, blocklength,
               vcv = var, window = c("rolling", "fixed"),
               bootstrap = c("moving", "circular"))
}

\arguments{
  \item{null}{A function that takes a subset of the data \code{dataset}
    as its argument and returns an object with a \code{predict} method.
    This function generates the benchmark forecast.}
  \item{alt}{A second function that takes a subset of the data \code{dataset}
    as its argument and returns an object with a \code{predict} method.
    This function generates the alternative forecast.}
  \item{alt.list}{A list of functions that would be valid as \code{alt}}
  \item{dataset}{A data frame.}
  \item{R}{An integer, the size of the training sample.  The asymptotic
    theory assumes that \code{R} is small.}
  \item{nboot}{An integer, the number of bootstrap replications.}
  \item{blocklength}{An integer, the length of the blocks for the moving
    or circular block bootstraps.}
  \item{vcv}{A function to calculate the asymptotic variance of the
    \acronym{oos} average.}
  \item{window}{A character that indicates the window strategy for
    \acronym{oos} estimation for the alternative model[s].  The
    benchmark model is always estimated with the recursive scheme.}
  \item{bootstrap}{Indicates whether to do the moving blocks bootstrap
    (MBB) (Kunsch, 1989 and Liu and Singh, 1992) or circular blocks
    bootstrap (CBB) (Politis and Romano, 1992)}
}
\details{Calhoun's (2011) mixed window \acronym{oos} test is a
  modification of Clark and West's (2006, 2007) that uses a recursive
  window for the benchmark model to ensure that the \acronym{oos}
  average is mean zero and asymptotically normal.  \code{mixedwindow}
  compares a pair of models and \code{mixedbootstrap} implements the
  bootstrap used for multiple comparisons.}

\value{\code{mixedwindow} returns a list with the following elements:
\item{mu}{The estimated \acronym{oos} average, which includes the
  adjustment for correct asymptotic centering}
\item{avar}{An estimate of the asymptotic variance of the \acronym{oos}
  average}
\item{pvalue}{The p-value of the test that the two models have
  equal population \acronym{mse} against the one-sided alternative that
  the alternative model is more accurate.}

\code{mixedbootstrap} returns an \code{length(alt.list)} by \code{nboot}
  matrix that contains the resampled values of the \acronym{oos} t-test
  based on \code{mixedwindow}.  These are the values of the t-statistic
  and not the test's p-values.
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

  Kunsch, H. R. 1989, The Jackknife and the Bootstrap for general
  stationary observations.  \emph{Annals of Statistics}, \bold{17}(3),
  pages 1217--1241.

  Liu, R. Y. and Kesar, S. 1992, Moving blocks Jackknife and Bootstrap
  capture weak dependence, in R. LePage and L. Billard, editors,
  \emph{Exploring the limits of Bootstrap}, John Wiley, pages 225--248.
  
  Politis, D. N. and Romano, J. P. 1992, A circular block-resampling
  procedure for stationary data, in R. LePage and L. Billard, editors,
  \emph{Exploring the limits of Bootstrap}, John Wiley, pages 263--270.

}
\author{Gray Calhoun \email{gcalhoun@iastate.edu}}

\seealso{\code{\link{dmw_calculation}}, \code{\link{clarkwest}},
  \code{\link{recursive_forecasts}}, \code{\link{predict}}, \code{\link{boot}}}

\examples{
n <- 30
R <- 5
d <- data.frame(y = rnorm(n), x1 = rnorm(n), x2 = rnorm(n))
model0 <- function(d) lm(y ~ 1, data = d)
model1 <- function(d) lm(y ~ x1, data = d)
model2 <- function(d) lm(y ~ x2, data = d)
model3 <- function(d) lm(y ~ x1 + x2, data = d)

mixedwindow(model0, model1, d, R, var, window = "rolling")

mixedbootstrap(model0, list(m1 = model1, m2 = model2, m3 = model3),
               d, R, 199, 7, var, "fixed", "circular")
}

\keyword{ts}
\keyword{htest}
\keyword{models}
