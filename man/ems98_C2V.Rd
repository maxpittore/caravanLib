\name{ems98_C2V}
\alias{ems98_C2V}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ems98_C2V(ems, type = 3)
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
function (ems, type = 3) 
{
    f <- c(0.22, 0.14, 0.1, 0.06, -0.08)
    e <- c(0.38, 0.3, 0.26, 0.22, 0.14)
    d <- c(0.54, 0.46, 0.4, 0.38, 0.3)
    c <- c(0.7, 0.62, 0.58, 0.54, 0.46)
    b <- c(0.86, 0.78, 0.74, 0.7, 0.62)
    a <- c(1.02, 0.94, 0.9, 0.86, 0.78)
    vmat <- rbind(a, b, c, d, e, f)
    vmat[ems, type][[1]]
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
