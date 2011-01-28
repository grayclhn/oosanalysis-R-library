\name{oosLoss-class}
\Rdversion{0.01}
\docType{class}
\alias{oosLoss-class}

\title{Class "oosLoss"}
\description{A collection of functions that define a data generating
  process for a linear regression and the conditional expectation of the
  loss that will arise from using that regression model to make
  forecasts (conditional on the parameter estimates).}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("oosLoss", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{ELoss}:}{Object of class \code{"function"} ~~ A function
      of the unknown (estimated) coefficients that returns the expected
      loss given those coefficient estimates.  \code{ELoss} must take a
      single numeric vector, \code{estimates}, as its argument and
      return a single numeric value.}
    \item{\code{RData}:}{Object of class \code{"function"} ~~ A function
      that generates a random draw of the associated DGP.  This function
      must take the arguments \code{nobs} and \{nreg} (the number of
    observations to draw  from the DGP and the number of regressors in
    the regression) and return a list with elements \code{X} and
    \code{Y}.  \code{X} is an \code{nobs}-by-\code{nreg} numeric matrix
    and \code{Y} is a numeric vector of length \code{nobs}.
    \item{\code{LossFn}:}{Object of class \code{"function"} ~~ The loss
      function that is used in ELoss to evaluate future forecasting
      performance.  This function must take and return a numeric vector
      (to support vectorization).}
  }
}
\section{Methods}{
  \describe{
    \item{E}{\code{signature(object = "oosLoss")}: Calls the
      function \code{ELoss}.}
    \item{E<-}{\code{signature(x = "oosLoss")}: Replaces the function
      called by \code{E}. }
    \item{L}{\code{signature(object = "oosLoss")}: Calls the
      function \code{RData}.}
    \item{L<-}{\code{signature(x = "oosLoss")}: Replaces the function
      called by \code{L}.}
    \item{r}{\code{signature(object = "oosLoss")}: ... Calls the
      function \code{LossFn}.}
    \item{r<-}{\code{signature(x = "oosLoss")}: Replaces the function
      called by \code{r}.}
  }
}
       
\references{Calhoun (2010) \emph{Limit Theory for Comparing Overfit
    Models Out-of-Sample}}

\seealso{\code{\link{oosLoss.init}}}

\examples{
showClass("oosLoss")
}
\keyword{classes}
