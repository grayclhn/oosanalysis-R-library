\name{mixedwindow}
\alias{mixedwindow}
\alias{mixedbootstrap}
\title{Calhoun's (2011) Out-of-Sample comparison}
\description{Implements the asymptotically normal OOS test statistics
  proposed by Calhoun (2011).}
\usage{
mixedwindow(null, alt, dataset, R, vcv = var, window = c("rolling", "fixed"))

mixedbootstrap(null, alt.list, dataset, R, nboot, blocklength,
               vcv = var, window = c("rolling", "fixed"),
               bootstrap = c("moving", "circular"))
}

\arguments{
  \item{null}{}
  \item{alt}{}
  \item{alt.list}{}
  \item{dataset}{}
  \item{R}{}
  \item{nboot}{An integer, the number of bootstrap replications.}
  \item{blocklength}{An integer, the length of the blocks for the moving
    or circular block bootstraps.}
  \item{vcv}{}
  \item{window}{}
  \item{bootstrap}{Indicates whether to do the moving blocks bootstrap
    (MBB) (Kunsch, 1989) or circular blocks bootstrap (CBB)
    (Politis and Romano, 19..)}
}
\details{text}
\value{text}
\references{
  Calhoun, G. 2011, An asymptotically normal out-of-sample test of equal
  predictive accuracy for nested models.  Unpublished manuscript.

  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.
}
\author{Gray Calhoun <gcalhoun@iastate.edu>}
\seealso{\code{\link{dmw.correction}}, \code{\link{clarkwest}}}
\examples{
## do something
}