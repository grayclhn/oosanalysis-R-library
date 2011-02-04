\name{nobs-methods}
\docType{methods}
\alias{nobs-methods}
\alias{nobs,data.frame-method}
\alias{nobs,zoo-method}
\title{Get number of observations}
\description{A generic function that gets the number of observations of
  an object}

\section{Methods}{
\describe{

\item{\code{signature(x = "data.frame")}}{
  Returns the number of rows in the data.frame.
}

\item{\code{signature(x = "zoo")}}{
  Returns the number of observations in 'x'.
}
}}
\keyword{methods}
