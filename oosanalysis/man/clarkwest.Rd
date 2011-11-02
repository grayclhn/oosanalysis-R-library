\name{clarkwest}
\alias{clarkwest}
\alias{clarkwest.calculation}
\title{Clark and West's (2006, 2007) Out-of-Sample Test}
\description{Builds their \acronym{oos} statistic}
\usage{
clarkwest(dataset, null, alt, R, yfn,
          window = c("rolling", "recursive", "fixed"), vcv = var)
clarkwest.calculation(target, null.forecast, alt.forecast)
}
\arguments{
  \item{dataset}{}
  \item{null}{}
  \item{alt}{}
  \item{R}{}
  \item{yfn}{}
  \item{window}{}
  \item{vcv}{}
  \item{target}{}
  \item{null.forecast}{}
  \item{alt.forecast}{}
}

\value{Both functions return the same thing, a list with elements
  \item{mu}{an estimate of the corrected \acronym{oos} mean,}
  \item{avar}{the asymptotic variance of the corrected \acronym{oos}
    average,}
  \item{pvalue}{the p-value associate with the one-sided \acronym{oos}
  test.}
}
\references{
  Calhoun, G. 2011, Documentation appendix: An asymptotically normal
  out-of-sample test of equal predictive accuracy for nested models.
  Unpublished manuscript.

  Clark, T. E., West, K. D. 2006, Using out-of-sample mean squared
  prediction errors to test the martingale difference hypothesis.
  \emph{Journal of Econometrics}, \bold{135}(1): 155--186.

  Clark, T. E., West, K. D. 2007, Approximately normal tests for equal
  predictive accuracy in nested models.  \emph{Journal of Econometrics},
  \bold{138}(1): 291--311.

}

\author{Gray Calhoun <galhoun@iastate.edu>}

\seealso{\code{\link{dmw.calculation}}, \code{\link{mixedwindow}}}
\examples{
## do something
}

\keyword{ts}
\keyword{htest}
\keyword{models}
