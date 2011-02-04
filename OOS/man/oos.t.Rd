\name{oos.t}
\alias{oos.t}
\alias{oos.t2}
\title{Out-of-sample t-test for one and two OOS periods}

\description{Calculates the out-of-sample t test.}

\usage{
oos.t(null.model, alt.model, data, R, L,
      window = c("rolling", "recursive", "fixed"),
      alternative = c("two.sided", "less", "greater"),
      method = c("DMW", "McC07"), conf.level = 0.95)
}

\arguments{
  \item{null.model}{A function that takes 'data' as an argument and
  returns an object with a 'predict' method.}
  \item{alt.model}{A function or a list of functions that takes 'data' as an argument and
  returns an object with a 'predict' method.}
  \item{data}{A data frame.}
  \item{R}{The number of observations in the estimation window.  This
  must be less than the number of observations in data.}
  \item{L}{A loss function that takes a single argument, the forecast error.}
  \item{window}{One of "rolling", "recursive", or "fixed", to indicate
  the estimation strategy.}
  \item{alternative}{Indicates whether the alternative is two sided or
  one sided.}
  \item{method}{Method for constructing critical values.  "DMW"
  indicates that we should use naive Gaussian critical values.  "McC07"
  uses critical values based on McCracken's (2007, Journal of
  Econometrics) "Asymptotics for out of sample tests of Granger causality."}
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

\examples{}