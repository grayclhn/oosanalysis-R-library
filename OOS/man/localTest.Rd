\name{localTest}
\alias{localTest}
\title{Test that null model is expected to be more accurate.}
\description{To do later.}
\usage{
localTest(y, x, x2.index, method = "homoskedastic")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{x2.index}{
%%     ~~Describe \code{x2.index} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
}
\details{To do later.}
\value{To do later.}
\references{
Todd E. Clark and Michael W. McCracken.  In-sample tests of predictive
ability: a new approach.  Research Working Paper RWP 09-10, Federal
Reserve Bank of Kansas City, 2009a.

Todd E. Clark and Michael W. McCracken.  Nested forecast model
comparisons: a new approach to testing equal accuracy.  Research Working
Paper RWP 09-11, Federal Reserve Bank of Kansas City, 2009b.}

\author{Gray Calhoun <gcalhoun@iastate.edu> and Xiaoqing Zhou}

\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

rmc <- function(n, b, sigma = 1) {
  k <- length(b)
  x <- cbind(1, matrix(rnorm(n * (k-1)), n, k-1))
  y <- x %*% b + rnorm(n, 0, sigma)
  list(y = y, x = x)
}

## quick simulation
n <- 300
nsim <- 1000

dmc <- replicate(1000, {
  d <- rmc(n, c(0,1))
  testfn(d$y, d$x, 2, "homoskedastic")
})

## estimated size of a test at 10%
mean(dmc / sd(dmc) > qnorm(.9))

}