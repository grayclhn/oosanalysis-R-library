\name{TestThis}
\alias{TestThis}
\title{
Quick Unit Test
}
\description{Evaluates some code and halts if it fails.  I use this for
  unit testing.
}
\usage{TestThis(x, message = "Failed the test.")}
\arguments{
  \item{x}{An object.}
  \item{message}{A character string containing the message to display of
    the test fails.}
}

\value{If \code{x} is \code{TRUE}, \code{TestThis} returns \code{TRUE}.
  Otherwise \code{TestThis} throws an error and displays \code{message}.
}

\examples{
## The function is very simple
TestThis

## should be fine
TestThis(TRUE)
## should also be fine
TestThis(all.equal(1:3, c(1, 2, 3)), "Can't count!")

## This line should fail
\dontrun{TestThis(1:3, "Can't count!")}
}
\keyword{error}
\keyword{debugging}
