\name{bootindex_circularblock}
\alias{bootindex_circularblock}
\alias{bootindex_movingblock}
\title{Indices to induce block bootstraps}
\description{These functions generate the random indices necessary to
  implement the moving blocks bootstrap (Kunsch, 1989) and the circular
  blocks bootstrap (Politis and Romano, 1994)}
\usage{
bootindex_movingblock(nobs, blocklength)
bootindex_circularblock(nobs, blocklength)
}

\arguments{
  \item{nobs}{The length of each bootstrap process.}
  \item{blocklength}{The block length.}
}
\details{Both the moving blocks bootstrap and the circular blocks
  bootstrap resample from the sequence X = X[1],\dots,X[n] by drawing
  length \code{blocklength} consecutive observations repeatedly and then
  pasting the blocks until the resampled sequence has the same length as
  the original.  The blocks are drawn independently of each other.  The
  circular block bootstrap allows (for example) X[n-1], X[n], X[1], X[2]
  to be a valid block of length 4, and the moving blocks bootstrap does
  not.  The functions \code{bootindex_movingblock} and
  \code{bootindex_circularblock} give the indices that induce the
  bootstrap, so for example we get a particular circular block bootstrap
  draw of \code{X} with block length 8 from the
  command \code{X[bootindex_movingblock(length(X), 8)]}.
  }

\value{A vector of indices that corresponds to a single bootstrap draw.}
\references{
  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.

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
\seealso{\code{\link{boot}}}
\examples{
## Example of hypothesis test that mean = 0
nobs <- 200
nboot <- 299
level <- .1
X <- 2 + arima.sim(n = nobs, list(ma = c(0.5)))

naive <- replicate(nboot, mean(X[sample(1:nobs, nobs, replace = TRUE)])) - mean(X)
smart <- replicate(nboot, mean(X[bootindex_circularblock(nobs, 5)])) - mean(X)

## corresponding critical values
quantile(naive, 1 - level)
quantile(smart, 1 - level)

\dontrun{
mc <- replicate(300, {
  X <- arima.sim(n = nobs, list(ma = c(0.5)))
  naive <- replicate(nboot, mean(X[sample(1:nobs, nobs, replace = TRUE)])) - mean(X)
  smart <- replicate(nboot, mean(X[bootindex_circularblock(nobs, 5)])) - mean(X)
  c(naive = mean(X) >= quantile(naive, 1 - level),
    smart = mean(X) >= quantile(smart, 1 - level))
  })
rowMeans(mc)
}
}

\keyword{htest}
\keyword{distribution}
\keyword{ts}