\name{bootindex.circularblock}
\alias{bootindex.circularblock}
\alias{bootindex.movingblock}
\title{Indices to induce block bootstraps}
\description{These functions generate the random indices necessary to
  implement the moving blocks bootstrap (Kunsch, 1989) and the circular
  blocks bootstrap (Politis and Romano, 1994)}
\usage{
bootindex.movingblock(nobs, blocklength)
bootindex.circularblock(nobs, blocklength)
}

\arguments{
  \item{nobs}{}
  \item{blocklength}{}
}
\details{text}
\value{A vector of indices.}
\references{
  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.

  Kunsch

  Politis and Romano
}
\author{Gray Calhoun <gcalhoun@iastate.edu>}
\seealso{\code{\link{boot}}}
\examples{
##---- Should be DIRECTLY executable !! ----
}

