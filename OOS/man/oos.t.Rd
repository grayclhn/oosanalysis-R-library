\name{oos.t}
\alias{oos.t}
\alias{oos.t2}
\title{Out-of-sample t-test for one and two OOS periods}

\description{Calculates the out-of-sample t test.}

\usage{
oos.t(null.model, alt.model, data, R,
      L = function(x) x^2,
      window = c("rolling", "recursive", "fixed"),
      method = c("DMW", "McC07"), alternative = "greater", 
      conf.level = 0.95)

oos.t2(null.model, alt.model, data,
       data2 = NULL, R, P2 = nobs(data2),
       L = function(x) x^2,
       window = c("rolling", "recursive", "fixed"),
       method = c("DMW", "McC07"), alternative = "greater",
       conf.level = 0.95)
}

\details{}

\arguments{

  \item{null.model}{A function that takes 'data' as an argument and
  returns an object with a 'predict' method.}

\item{alt.model}{A function or a list of functions that takes 'data' as an argument and
  returns an object with a 'predict' method.}

\item{data}{A data frame.}

\item{data2}{A second data frame with the same variables as the first, to
  calculate a second out-of-sample average.}

\item{R}{The number of observations in the estimation window.  This
  must be less than the number of observations in data.}

\item{P2}{The number of observations in the second test sample.  When
  data2 is supplied, this will typically be the number of observations
  in data2.  But when data2 is not supplied, P2 should be the number of
  observations in a hypothetical second dataset.}

\item{L}{A loss function that takes a single argument, the forecast error.}

\item{window}{One of "rolling", "recursive", or "fixed", to indicate
  the estimation strategy.}

\item{method}{Method for constructing critical values.  "DMW"
  indicates that we should use naive Gaussian critical values.  "McC07"
  uses critical values based on McCracken's (2007, Journal of
  Econometrics) "Asymptotics for out of sample tests of Granger causality."}

\item{alternative}{One of "two.sided", "less", or "greater",
    indicating whether the alternative is two sided or
  one sided.  Note that the null hypothesis is \deqn{E L(m.null) - E
  L(m.alt) = 0} so you almost always want the alternative \deqn{E
  L(m.null) - E L(m.alt) \geq 0,} which corresponds to "greater" and
  means that the alternative is more accurate.}

\item{conf.level}{Confidence level for confidence intervals.}
}

\value{
  An object of type 'htest' (which is returned by t.test too), or a list
  of objects of type 'htest', depending on whether or not 'alt.model' is
  a list.
}
\references{
  F. X. Diebold and R.S. Mariano. Comparing predictive
  accuracy. \emph{Journal of Business and Economic Statistics},
  13(3):253-263, 1995.
  
  M. W. McCracken. Asymptotics for out of sample tests of Granger
  causality. \emph{Journal of Econometrics}, 140(2):719-752, Oct. 2007.

  K. D. West. Asymptotic inference about predictive
  ability. \emph{Econometrica}, 64(5):1067-1084, Sept. 1996.
}
\author{Gray Calhoun <gcalhoun@iastate.edu>}

\seealso{\code{\link{apply.oos}},\code{\link{t.test}}}

\examples{
check <- function(a,b,...) stopifnot(all.equal(a,b,check.attributes = FALSE,...))

L <- function(x) x^2
d <- data.frame(x = rnorm(20), y = rnorm(20), z = rnorm(20))
R <- 15

null.model <- function(d) lm(y ~ 1, data = d)
alt.modelx <- function(d) lm(y ~ x, data = d)
alt.modelz <- function(d) lm(y ~ z, data = d)

L.null <- L(apply.oos(R, d, null.model, "rolling", "error"))
L.alt  <- L(apply.oos(R, d, alt.modelx, "rolling", "error"))

tstat <- t.test(L.null - L.alt, alt = "greater")
oos.tstat <- oos.t(null.model, alt.modelx, d, R, L, "rolling", "DMW")
oos.tstat2 <- oos.t2(null.model, alt.modelx, d, R = R, P2 = Inf, L = L,
                     window = "rolling", method = "DMW")


check(tstat$statistic, oos.tstat$statistic)
check(tstat$statistic, oos.tstat2$statistic)

check(tstat$p.value,   oos.tstat$p.value)
check(tstat$p.value,   oos.tstat2$p.value)

check(tstat$conf.int,  oos.tstat$conf.int)
check(tstat$conf.int,  oos.tstat2$conf.int)

check(tstat$estimate,  oos.tstat$estimate)
check(tstat$estimate,  oos.tstat2$estimate)

oos.t(null.model, list(a = alt.modelx, y = alt.modelz),
      d, R, L, "rolling", "DMW", "greater")

d2 <- data.frame(x = rnorm(60), y = rnorm(60))
oos.t2(null.model, alt.modelx, d, data2 = d2, R = R, L = L,
       window = "rolling", method = "DMW")

## works with dynamic models too!
library(dyn)
dyn.model <- function(d) lm(y ~ lag(y, -2), data = d)
oos.t2(null.model, dyn.model, d, data2 = d2, R = R, L = L,
       window = "rolling", method = "DMW")
}