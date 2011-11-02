\name{bootindex.circularblock}
\alias{bootindex.circularblock}
\alias{bootindex.movingblock}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Indices to induce block bootstraps}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
bootindex.movingblock(nobs, blocklength)
bootindex.circularblock(nobs, blocklength)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nobs}{
%%     ~~Describe \code{nobs} here~~
}
  \item{blocklength}{
%%     ~~Describe \code{blocklength} here~~
}
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
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(nobs, blocklength) {
  blockstarts <- sample(seq_len(nobs), ceiling(nobs / blocklength))
  as.vector(sapply(blockstarts, function(s)
    (s + seq_len(blocklength) - 1) \%\% nobs + 1))[seq_len(nobs)]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
