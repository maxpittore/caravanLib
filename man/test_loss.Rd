\name{test_loss}
\alias{test_loss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
test the functions for computing loss (deprecated)
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
test_loss(type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (type) 
{
    path <- "/home/max/Documents/GFZ_sync/workspace/yurta/"
    file <- sprintf("\%s\%s", path, "Intensities/intensities.tif")
    GDALinfo(file)
    shakemap_gdal = readGDAL(file)
    shakemap <- raster(shakemap_gdal)
    dbconn <- "PG:dbname=test_db host='localhost' user='postgres' password='postgres'"
    dbconn2 <- "PG:dbname=centralasia_db host='lhotse.gfz-potsdam.de' user='postgres' password='postgres'"
    if (type == 1) {
        dbin <- list(dbconn = dbconn2, layer = "places", overwrite = FALSE)
        dbout <- list(dbconn = dbconn2, layer = "places_loss", 
            overwrite = TRUE)
        compute_loss_points(shakemap, nighttime = FALSE, dbin, 
            dbout)
    }
    else if (type == 2) {
        dbin <- list(dbconn = dbconn2, layer = "urbanstructures", 
            overwrite = FALSE)
        dbout <- list(dbconn = dbconn2, layer = "urbanstructures_loss", 
            overwrite = TRUE)
        compute_loss_geocells(shakemap, nighttime = FALSE, dbin, 
            dbout)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
