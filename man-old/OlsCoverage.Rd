\name{OlsCoverage}
\alias{OlsCoverage}
\alias{OlsPerformanceVals}
\title{
Determines Coverage for Out-of-Sample Confidence Intervals
}
\description{
Quick functions to generate quantities needed to determine the coverage
of out-of-sample confidence intervals for OLS, for all of the different possible
values of R.
}
\usage{
OlsCoverage(x, X, Y, min.R, nreg1, size)
OlsPerformanceVals(x, X, Y, min.R, nreg1)
}
\arguments{
  \item{x}{An object of class \code{oosLoss}.}
  \item{X}{A numeric matrix representing the OLS design matrix.}
  \item{Y}{A numeric vector giving the dependent variable.}
  \item{min.R}{The minimum training sample to be considered.}
  \item{nreg1}{The number of regressors that the smaller benchmark model
    will use.}
  \item{size}{A vector listing the sizes of the confidence intervals
    under consideration.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\seealso{
  \code{"\linkS4class{oosLoss}"}
}
\examples{
}