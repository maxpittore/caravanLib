\name{ems98_DPM}
\alias{ems98_DPM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ems98_DPM(ems, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ems}{
%%     ~~Describe \code{ems} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
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
function (ems, type) 
{
    V <- ems98_C2V(ems, type)
    mus <- sapply(seq(5, 10), function(x) rep(ems98_DG(x, V), 
        6))
    ind <- rep(seq(0, 5), 6)
    d <- sapply((1:36), function(x) P_DS(ind[x], mus[x]))
    dpm <- matrix(d, nrow = 6, ncol = 6, byrow = TRUE, dimnames = list(c("V", 
        "VI", "VII", "VIII", "IX", "X"), c("d0", "d1", "d2", 
        "d3", "d4", "d5")))
    dpm
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
