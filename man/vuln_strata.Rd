\name{vuln_strata}
\alias{vuln_strata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
vuln_strata()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    sql <- "select structure as s, unnest(post_vuln) as vuln from avg_posteriors;"
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host = "lhotse.gfz-potsdam.de", user = "postgres", 
        password = "postgres", dbname = "centralasia_db")
    post <- dbGetQuery(con, statement = sql)
    dbDisconnect(con)
    dbUnloadDriver(drv)
    t(sapply(1:21, FUN = function(x) {
        post$vuln[post$s == x]
    }))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
