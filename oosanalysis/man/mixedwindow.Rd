\name{mixedwindow}
\alias{mixedwindow}
\title{Calhoun's (2011) Out-of-Sample comparison}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mixedwindow(null, alt, dataset, R, vcv = var, window = c("rolling", "fixed"))

mixedbootstrap(null, alt.list, dataset, R, nboot, blocklength,
               vcv = var, window = c("rolling", "fixed"),
               bootstrap = c("moving", "circular"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{null}{
%%     ~~Describe \code{null} here~~
}
  \item{alt}{
%%     ~~Describe \code{alt} here~~
}
  \item{dataset}{
%%     ~~Describe \code{dataset} here~~
}
  \item{R}{
%%     ~~Describe \code{R} here~~
}
\item{nboot}{An integer, the number of bootstrap replications.}
\item{blocklength}{An integer, the length of the blocks for the moving
               or circular block bootstraps.}
  \item{vcv}{
%%     ~~Describe \code{vcv} here~~
}
\item{window}{
%%     ~~Describe \code{window} here~~
}
\item{bootstrap}{Indicates whether to do the moving blocks bootstrap
               (MBB) (Kunsch, 1989) or circular blocks bootstrap (CBB)
               (Politis and Romano, 19..)}
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
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{}