\name{dmw_mse}
\alias{dmw_mse}
\alias{dmw_calculation}
\alias{mixedwindow}
\alias{mixedbootstrap}
\title{Diebold-Mariano-West out-of-sample t-test}

\description{The Diebold-Mariano-West \acronym{oos} t-test can be used
  to compare population forecasting models under some fairly restrictive
  circumstances (see West, 2006).  The forecast are assumed to be
  constructed using a fixed, recursive, or rolling estimation window and
  depend on the estimated coefficients \eqn{\hat\beta_t}.  The function
  \code{dmw_calculation} takes as arguments the matrices and vectors
  that West (1996) and West and McCracken (1998) use to represent the
  asymptotic distribution of this statistic and just assembles the mean
  and variance components of the statistic.  \code{dmw_mse} is a basic
  convenience wrapper for the common use case: squared error loss with
  least squares forecasts.  The \code{mixedwindow} functions implement
  the asymptotically normal OOS test statistics proposed by Calhoun
  (2011).
}
  
\usage{
dmw_mse(null, alt, dataset, R, vcv = var,
        window = c("recursive", "rolling", "fixed"),
        pimethod = "estimate")

dmw_calculation(f, h, R, vcv, tBtF = NULL, pi = noos / R,
                window = c("recursive", "rolling", "fixed"))

mixedwindow(null, alt, dataset, R, vcv = var,
            window = c("rolling", "fixed"), pimethod = "estimate")

mixedbootstrap(null, alt.list, dataset, R, nboot, blocklength,
               vcv = var, window = c("rolling", "fixed"),
               bootstrap = c("moving", "circular", "stationary"),
               pimethod = "estimate")
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

  \item{f}{A vector containing the \acronym{oos} observations}

  \item{h}{A matrix containing something like (for \acronym{ols} using
  the obvious notation) \eqn{x_t \varepsilon_t} for \eqn{t} ranging over
  the \acronym{oos} period.}

\item{tBtF}{A vector that represents \eqn{B'F'} in West's (1996)
    notation.  This term captures the uncertainty introduced by
    estimating the unknown model coefficients; if the coefficients are
    known or imposed, instead of estimated, set this argument to
    \code{NULL}}

\item{pi}{A numeric scalar, the ratio of the number of out-of-sample
    observations to the number of training sample observations.
    \code{noos} is defined in the body of the function as \code{length(f)}.}
  
  \item{window}{A character string indicating which window strategy was
    used to generate the \acronym{oos} observations.  For the
    \code{mixedwindow} functions, this is the window strategy for
    \acronym{oos} estimation for the alternative model[s] since the
    benchmark model is always estimated with the recursive scheme.}

  \item{nboot}{An integer, the number of bootstrap replications.}

  \item{blocklength}{An integer, the length of the blocks for the moving
    or circular block bootstraps.}

  \item{vcv}{A function to calculate the asymptotic variance of the
    \acronym{oos} average.}

  \item{pimethod}{Indicates whether Pi (= lim P/R) should be estimated
    as P/R (\code{pimethod = "estimate"}) or set to the theoretical limit of
  infinity (\code{pimethod = "theory"}). }
  \item{bootstrap}{Indicates whether to do the moving blocks bootstrap
    (MBB) (Kunsch, 1989 and Liu and Singh, 1992), circular blocks
    bootstrap (CBB) (Politis and Romano, 1992), or stationary bootstrap
  (Politis and Romano, 1994)}
}

\details{Calhoun's (2011) mixed window \acronym{oos} test is a
  modification of Clark and West's (2006, 2007) that uses a recursive
  window for the benchmark model to ensure that the \acronym{oos}
  average is mean zero and asymptotically normal.  \code{mixedwindow}
  compares a pair of models and \code{mixedbootstrap} implements the
  bootstrap used for multiple comparisons.}

\value{\code{dmw_mse} and \code{dmw_calculation} each return a list
  containing the following elements:

  \item{mu}{The \acronym{oos} average,}

  \item{avar}{The asymptotic variance of the \acronym{oos} average.}

  \code{mixedwindow} returns a list with the following elements:

  \item{mu}{The estimated \acronym{oos} average, which includes the
    adjustment for correct asymptotic centering}
  
\item{avar}{An estimate of the asymptotic variance of the \acronym{oos}
  average}

\item{pvalue}{The p-value of the test that the two models have equal
  population \acronym{mse} against the one-sided alternative that the
  alternative model is more accurate.}

\code{mixedbootstrap} returns an \code{length(alt.list)} by \code{nboot}
  matrix that contains the resampled values of the \acronym{oos} t-test
  based on \code{mixedwindow}.  These are the values of the t-statistic
  and not the test's p-values.
}

\references{
  Calhoun, G. 2011, An asymptotically normal out-of-sample test of equal
  predictive accuracy for nested models.  Unpublished manuscript.

  Calhoun, G. 2011, Supplemental appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.

  Clark, T. E., West, K. D. 2006, Using out-of-sample mean squared
  prediction errors to test the martingale difference hypothesis.
  \emph{Journal of Econometrics}, \bold{135}(1): 155--186.

  Clark, T. E., West, K. D. 2007, Approximately normal tests for equal
  predictive accuracy in nested models.  \emph{Journal of Econometrics},
  \bold{138}(1): 291--311.

  Diebold, F. X. and Mariano, R. S. 1995, Comparing predictive accuracy.
  \emph{Journal of Business and Economic Statistics}, \bold{138}(1):
  253--263.
  
  Kunsch, H. R. 1989, The Jackknife and the Bootstrap for general
  stationary observations.  \emph{Annals of Statistics}, \bold{17}(3),
  pages 1217--1241.

  Liu, R. Y. and Kesar, S. 1992, Moving blocks Jackknife and Bootstrap
  capture weak dependence, in R. LePage and L. Billard, editors,
  \emph{Exploring the limits of Bootstrap}, John Wiley, pages 225--248.
  
  Politis, D. N. and Romano, J. P. 1992, A circular block-resampling
  procedure for stationary data, in R. LePage and L. Billard, editors,
  \emph{Exploring the limits of Bootstrap}, John Wiley, pages 263--270.

  Politis, D. N. and Romano, J. P. 1994, The Stationary Bootstrap.
  \emph{Journal of the American Statistical Association},
  \bold{89}(428), pages 1303-1313.

  West, K. D. 1996, Asymptotic inference about predictive ability.
  \emph{Econometrica}, \bold{64}(5): 1067--1084.

  West, K. D. 2006, Forecast evaluation, in G. Elliott, C. Granger, and
  A. Timmermann, editors, \emph{Handbook of Economic Forecasting},
  volume 1, pages 99--134. Elsevier.

  West, K. D. and McCracken, M. W. 1998, Regression-based tests of
  predicitve ability.  \emph{International Economic Review},
  \bold{39}(4):817--840.
}
\author{Gray Calhoun \email{gcalhoun@iastate.edu}}

\seealso{\code{\link{clarkwest}},
  \code{\link{mccracken_criticalvalue}},
  \code{\link{recursive_forecasts}}, \code{\link{predict}},
  \code{\link{boot}}}

\examples{
x <- rnorm(100)
d <- data.frame(y = x + rnorm(100), x = x)
R <- 70
oos <- 71:100

error.model1 <- d$y[oos] - predict(lm(y ~ 1, data = d[-oos,]),
                                   newdata = d[oos,])
error.model2 <- d$y[oos] - predict(lm(y ~ x, data = d[-oos,]),
                                   newdata = d[oos,])
# test that the two models have equal population MSE.  Note that F = 0
# in this setting.
estimates <-
  dmw_calculation(error.model1^2 - error.model2^2,
                  cbind(error.model1, error.model2, error.model2 * x),
                  R = R, vcv = var)
# calculate p-value for a one-sided test
pnorm(estimates$mu * sqrt(length(oos) / estimates$avar))


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
