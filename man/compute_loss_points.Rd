\name{compute_loss_points}
\alias{compute_loss_points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute fatality range for locations (settlements) based on a shakemap
%%  ~~function to do ... ~~
}
\description{
estimate the loss of a set of settlements in term of expected fatalities, based on a shakemap in macroseismic intensity provided as raster. 
Saves a shapefile with the settlements exposed to mmi > 5. for each settlements the bounds for the fatalities are provided, in absolute and relative terms
 dbin, dbout: list(db_conn,layer,overwrite) to specify input and output layers in the dbs
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
compute_loss_points(shakemap, nighttime = FALSE, dbin, dbout)
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
    vuln_emca <- c("b", "b", "b", "d", "d", "e", "e", "c", "d", 
        "a", "c", "d", "e")
    mmi <- c("I", "II", "III", "IV", "V", "VI", "VII", "VIII", 
        "IX", "X")
    occupancy <- c(28, 8, 140, 60, 200, 200, 200, 200, 140, 5, 
        28, 5, 5)
    nighttime_coeff <- 0.5
    daytime_coeff <- 0.3
    nclass <- 13
    off <- 4
    coeff <- ifelse(nighttime, nighttime_coeff, daytime_coeff)
    data <- readOGR(dsn = dbin$dbconn, layer = dbin$layer)
    if (is.na(proj4string(data))) {
        print("no projection on input file, assigning WGS84")
        proj4string(data) <- "+proj=longlat +datum=WGS84 +no_defs"
    }
    places <- subset(data, data$population > 0 & data$population < 
        1e+06)
    intens <- round(extract(shakemap, places@coords, method = "bilinear"))
    ind <- which(intens > 5)
    loss <- matrix(ncol = 6, nrow = length(ind))
    tot_loss_min <- 0
    tot_loss_med <- 0
    tot_loss_max <- 0
    i <- 1
    for (place in ind) {
        n_fat_min <- 0
        n_fat_med <- 0
        n_fat_max <- 0
        for (type in 1:nclass) {
            dpm <- ems98_DPM(vuln_emca[type], 3)
            pd4 <- dpm[mmi[intens[place]], "d4"]
            pd5 <- dpm[mmi[intens[place]], "d5"]
            b_min <- (places@data)[place, off + type]
            b_med <- (places@data)[place, off + nclass + type]
            b_max <- (places@data)[place, off + 2 * nclass + 
                type]
            pd <- (pd5 + 0.25 * pd4)
            n_coll_min <- pd * b_min
            n_coll_med <- pd * b_med
            n_coll_max <- pd * b_max
            n_fat_min <- n_fat_min + (occupancy[type] * coeff * 
                n_coll_min)
            n_fat_med <- n_fat_med + (occupancy[type] * coeff * 
                n_coll_med)
            n_fat_max <- n_fat_max + (occupancy[type] * coeff * 
                n_coll_max)
        }
        loss[i, 1] <- round(n_fat_min)
        loss[i, 2] <- round(n_fat_med)
        loss[i, 3] <- round(n_fat_max)
        tot_loss_min <- tot_loss_min + loss[i, 1]
        tot_loss_med <- tot_loss_med + loss[i, 2]
        tot_loss_max <- tot_loss_max + loss[i, 3]
        pop <- (places@data)$population[place]
        loss[i, 4] <- n_fat_min/pop
        loss[i, 5] <- n_fat_med/pop
        loss[i, 6] <- n_fat_max/pop
        i <- i + 1
    }
    df2 <- data.frame(osm_id = places@data$osm_id[ind], name = places@data$name[ind], 
        population = places@data$population[ind], loss = loss)
    df3 <- SpatialPointsDataFrame(places[ind, ], df2)
    writeOGR(df3, dbout$dbconn, dbout$layer, "PostgreSQL", overwrite_layer = dbout$overwrite)
    return(list(totloss_min = tot_loss_min, totloss_med = tot_loss_med, 
        totloss_max = tot_loss_max))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
