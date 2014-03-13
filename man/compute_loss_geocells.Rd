\name{compute_loss_geocells}
\alias{compute_loss_geocells}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compute_loss_geocells(shakemap, nighttime = FALSE, dbin, dbout)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shakemap}{
%%     ~~Describe \code{shakemap} here~~
}
  \item{nighttime}{
%%     ~~Describe \code{nighttime} here~~
}
  \item{dbin}{
%%     ~~Describe \code{dbin} here~~
}
  \item{dbout}{
%%     ~~Describe \code{dbout} here~~
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
function (shakemap, nighttime = FALSE, dbin, dbout) 
{
    nighttime_coeff <- 0.5
    daytime_coeff <- 0.3
    coeff <- ifelse(nighttime, nighttime_coeff, daytime_coeff)
    tmpdata <- readOGR(dsn = dbin$dbconn, layer = dbin$layer)
    tmpdata <- spTransform(tmpdata, CRS("+proj=longlat +datum=WGS84 +no_defs"))
    geocells <- subset(tmpdata, tmpdata@data$structure > 0)
    geocells_centroids <- coordinates(geocells)
    intens <- round(extract(shakemap, geocells_centroids, method = "bilinear"))
    ind <- which(intens > 5)
    vuln_strata <- vuln_strata()
    type <- 3
    dpm_a <- ems98_DPM("a", type)
    dpm_b <- ems98_DPM("b", type)
    dpm_c <- ems98_DPM("c", type)
    dpm_d <- ems98_DPM("d", type)
    dpm_e <- ems98_DPM("e", type)
    dpm_f <- ems98_DPM("f", type)
    process_geocells <- function(cell) {
        stratum <- geocells[cell, ]@data$structure
        nb <- round(geocells[cell, ]@data$nbuildings_mean)
        ncel <- 0
        ncol <- dpm_a[intens[cell] - 4, "d5"] * vuln_strata[stratum, 
            1]
        ncol <- ncol + dpm_b[intens[cell] - 4, "d5"] * vuln_strata[stratum, 
            2]
        ncol <- ncol + dpm_c[intens[cell] - 4, "d5"] * vuln_strata[stratum, 
            3]
        ncol <- ncol + dpm_d[intens[cell] - 4, "d5"] * vuln_strata[stratum, 
            4]
        ncol <- ncol + dpm_e[intens[cell] - 4, "d5"] * vuln_strata[stratum, 
            5]
        ncol <- ncol + dpm_f[intens[cell] - 4, "d5"] * vuln_strata[stratum, 
            6]
        ncol <- round(ncol * nb)
        avg_occ <- 10
        nfat <- ncol * avg_occ * coeff
    }
    est_fat <- (sapply(ind, FUN = process_geocells))
    df2 <- data.frame(geocells[ind, ]@data, est_fat)
    df3 <- SpatialPolygonsDataFrame(geocells[ind, ], df2)
    writeOGR(df3, dbout$dbconn, dbout$layer, "PostgreSQL", overwrite_layer = dbout$overwrite)
    return(tot_fatalities = sum(est_fat))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
