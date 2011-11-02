\name{generate.data.mc1}
\alias{generate.data.mc1}
\alias{generate.data.mc2}
\title{Generate data for monte carlo}
\description{Functions to generate data that I'll use in my Monte Carlo.}

\usage{
generate.data.mc1(n, isPower)
generate.data.mc2(n, isPower, nburn = 1600)
}
\arguments{
  \item{n}{}
  \item{isPower}{}
  \item{nburn}{}
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

\examples{
data1 <- generate.data.mc1(100, 0)
data2 <- generate.data.mc2(100, 0)
}