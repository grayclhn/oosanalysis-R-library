\name{localTest}
\alias{localTest}
\title{Test that null model is expected to be more accurate.}
\description{To do later.}
\usage{
localTest(y, x, x2.index,
          method = c("homoskedastic", "heteroskedastic"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{A numeric vector: the dependent variable in a linear regression model.}
  \item{x}{A numeric matirx: the matrix of regressors.}
  \item{x2.index}{An integer or logical vector that indicates which
    variables are missing from the benchmark model.}
  \item{method}{How should we handle heteroskedasticity and dependence?
    "homoskedastic" indicates that the regression errors are
    uncorrelated and homoskedastic.  "heteroskedastic" indicates that
    they are uncorrelated but conditionally heteroskedastic.}
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
n <- 40
k <-  3

x <- cbind(1, matrix(rnorm(n * (k-1)), n, k-1))
y <- rnorm(n)

localTest(y, x, c(3), "homoskedastic")
localTest(y, x, c(3), "heteroskedastic")

## quick simulation
n <- 300
nsim <- 1000

dmc <- replicate(1000, {
  x <- cbind(1, matrix(rnorm(n * (k-1)), n, k-1))
  y <- rnorm(n)
  localTest(y, x, 2, "hom")
})

## estimated size of a test at 10%
mean(dmc / sd(dmc) > qnorm(.9))

}