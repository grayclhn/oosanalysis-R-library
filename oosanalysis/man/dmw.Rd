\name{dmw.calculation}
\alias{dmw.calculation}
\title{Diebold-Mariano-West out-of-sample t-test}

\description{The Diebold-Mariano-West \acronym{oos} t-test can be used
    to compare population forecasting models under some fairly
    restrictive circumstances (see West, 2006).  The forecast are
    assumed to be constructed using a fixed, recursive, or rolling
    estimation window and depend on the estimated coefficients
    \eqn{\hat\beta_t}.  The function \code{dmw.calculation} takes as
    arguments the matrices and vectors that West (1996) and West and
    McCracken (1998) use to represent the asymptotic distribution of
    this statistic and just assembles the mean and variance components
    of the statistic.}


\usage{
dmw.calculation(f, h, R, vcv, tBtF = NULL,
                window = c("recursive", "rolling", "fixed"))
}
\arguments{
  \item{f}{A vector containing the \acronym{oos} observations}

  \item{h}{A matrix containing something like (for \acronym{ols} using
  the obvious notation) \eqn{x_t \varepsilon_t} for \eqn{t} ranging over
  the \acronym{oos} period.}

  \item{R}{The number of observations in the training sample.}

  \item{vcv}{A function to calculate asymptotic variance of \eqn{(\bar
      f^*, \bar h^*)}, the \acronym{oos} sequences evaluated at the true
      parameter values.}

  \item{tBtF}{A vector that represents \eqn{B'F'} in West's (1996)
    notation.  This term captures the uncertainty introduced by
    estimating the unknown model coefficients; if the coefficients are
    known or imposed, instead of estimated, set this argument to
    \code{NULL}}

  \item{window}{A character string indicating which window strategy was
    used to generate the \acronym{oos} observations.}
}

\value{A list containing the following elements:
  \item{mu}{The \acronym{oos} average,}
  \item{avar}{The asymptotic variance of the \acronym{oos} average.}
}

\references{

  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.

  Diebold, F. X. and Mariano, R. S. 1995, Comparing predictive accuracy.
  \emph{Journal of Business and Economic Statistics}, \bold{138}(1):
  253--263.
  
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

\seealso{\code{\link{mixedwindow}}, \code{\link{clarkwest}},
  \code{\link{recursive.forecasts}}, \code{\link{predict}}} 
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
  dmw.calculation(error.model1^2 - error.model2^2,
                  cbind(error.model1, error.model2, error.model2 * x),
                  R = R, vcv = var)
# calculate p-value for a one-sided test
pnorm(estimates$mu * sqrt(length(oos) / estimates$avar))
}

\keyword{ts}
\keyword{htest}
\keyword{models}
