\name{EastAuClimate}
\alias{EastAuClimate}
\docType{data}
\title{ Climate of the East Coast of Australia }
\description{
  A set of climate statistics for 16 coastal locations along Eastern
  Australia. These sites were chosen to be approximately equally
  spaced to cover the whole eastern coast of Australia.
  For each site, climate statistics were calculated for the standard
  30-year period 1971-2000. Only sites with nearly-complete data were
  chosen.
}
\usage{data(EastAuClimate)}
\format{
  A data frame with the following 10 variables and 5 items of metadata
  for each of 16 sites.
  \describe{
    \item{\code{SummerMaxTemp}}{ average daily maximum air
      temperature (degrees C) in February. }
    \item{\code{SummerMinTemp}}{ average daily minimum air
      temperature (degrees C) in February. }
    \item{\code{WinterMaxTemp}}{ average daily maximum air
      temperature (degrees C) in July. }
    \item{\code{WinterMinTemp}}{ average daily minimum air
      temperature (degrees C) in July. }
    \item{\code{SummerRain}}{ median total precipitation in February
      (mm/month). }
    \item{\code{WinterRain}}{ median total precipitation in July
      (mm/month). }
    \item{\code{MeanAnnRain}}{ average total amount of precipitation
      recorded in a year (mm/year). }
    \item{\code{RainDays}}{ average number of days in a year with at
      least 1 mm of precipitation. }
    \item{\code{ClearDays}}{ average number of clear days in a
      year. This statistic is derived from cloud cover observations,
      which are measured in oktas (eighths). A clear day is recorded
      when the mean of the 9 am and 3 pm cloud observations is less than
      or equal to 2 oktas. }
    \item{\code{CloudyDays}}{ average number of clear days in a
      year. A cloudy day is recorded when the mean of the 9 am and 3 pm
      cloud observations is greater than or equal to 6 oktas. }
    \item{\code{ID}}{ BOM Site number. }
    \item{\code{Latitude}}{ Site latitude (degrees North). }
    \item{\code{Longitude}}{ Site longitude (degrees East). }
    \item{\code{Elevation}}{ Site elevation (m). }
    \item{\code{State}}{ Australian state:
      TAS = Tasmania, VIC = Victoria, NSW = New
      South Wales, QLD = Queensland. }
  }

  The row names of the data frame give the location names. Note: these
  are not the official names of the climate stations.
}
\source{
  Sites were chosen by hand from maps on the Bureau of Meteorology
  website. The data were extracted manually from web pages under
  \url{http://www.bom.gov.au/climate/averages/}
  and processed to extract a subset of statistics.
  - by Felix Andrews \email{felix@nfrac.org}

  Bureau of Meteorology, Commonwealth of Australia.
  Product IDCJCM0026 Prepared at Wed 31 Dec 2008.

  Definitions of statistics adapted from
  \url{http://www.bom.gov.au/climate/cdo/about/about-stats.shtml}
}
%map(regions = c("Australia", "Australia:Tasmania"),
%          exact = TRUE, xlim = c(130, 170))
%with(EastAuClimate, {
%  points(Longitude, Latitude, pch = 16)
%  text(Longitude, Latitude, row.names(EastAuClimate),
%       pos = c(4, 4, 2, 2, rep(4, 12)))
%})
\examples{
data(EastAuClimate)

## Compare the climates of state capital cities
EastAuClimate[c("Hobart", "Melbourne", "Sydney", "Brisbane"),]

## A function to plot maps (a Lattice version of maps::map)
lmap <-
   function(database = "world", regions = ".", exact = FALSE,
            boundary = TRUE, interior = TRUE, projection = "",
            parameters = NULL, orientation = NULL,
            aspect = "iso", type = "l",
            par.settings = list(axis.line = list(col = "transparent")),
            xlab = NULL, ylab = NULL, ...)
{
   theMap <- map(database, regions, exact = exact,
                 boundary = boundary, interior = interior,
                 projection = projection, parameters = parameters,
                 orientation = orientation, plot = FALSE)
   xyplot(y ~ x, theMap, type = type, aspect = aspect,
          par.settings = par.settings, xlab = xlab, ylab = ylab,
          default.scales = list(draw = FALSE), ...)
}

## Plot the sites on a map of Australia
if (require("maps")) {
  lmap(regions = c("Australia", "Australia:Tasmania"),
       exact = TRUE, projection = "rectangular",
       parameters = 150, xlim = c(130, 170),
       panel = function(...) {
          panel.xyplot(...)
          with(EastAuClimate, {
            panel.points(Longitude, Latitude, pch = 16)
            txt <- row.names(EastAuClimate)
            i <- c(3, 4)
            panel.text(Longitude[ i], Latitude[ i], txt[ i], pos = 2)
            panel.text(Longitude[-i], Latitude[-i], txt[-i], pos = 4)
          })
       })
}

## Average daily maximum temperature in July (Winter).
xyplot(WinterMaxTemp ~ Latitude, EastAuClimate, aspect = "xy",
       type = c("p", "a"), ylab = "Temperature (degrees C)")

## (Make a factor with levels in order - by coastal location)
siteNames <- factor(row.names(EastAuClimate),
           levels = row.names(EastAuClimate))
## Plot temperature ranges (as bars), color-coded by RainDays
segplot(siteNames ~ WinterMinTemp + SummerMaxTemp, EastAuClimate,
        level = RainDays, sub = "Color scale: number of rainy days per year",
        xlab = "Temperature (degrees C)",
        main = paste("Typical temperature range and wetness",
           "of coastal Australian cities", sep = "\n"))

## Show Winter and Summer temperature ranges separately
segplot(Latitude ~ WinterMinTemp + SummerMaxTemp, EastAuClimate,
   main = "Average daily temperature ranges \n of coastal Australian sites",
   ylab = "Latitude", xlab = "Temperature (degrees C)",
   par.settings = simpleTheme(lwd = 3, alpha = 0.5),
   key = list(text = list(c("July (Winter)", "February (Summer)")),
              lines = list(col = c("blue", "red"))),
   panel = function(x, y, z, ..., col) {
      with(EastAuClimate, {
         panel.segplot(WinterMinTemp, WinterMaxTemp, z, ..., col = "blue")
         panel.segplot(SummerMinTemp, SummerMaxTemp, z, ..., col = "red")
      })
   })

## Northern sites have Summer-dominated rainfall;
## Southern sites have Winter-dominated rainfall.
xyplot(SummerRain + WinterRain ~ Latitude, EastAuClimate,
       type = c("p", "a"), auto.key = list(lines = TRUE),
       ylab = "Rainfall (mm / month)")

## Clear days are most frequent in the mid latitudes.
xyplot(RainDays + CloudyDays + ClearDays ~ Latitude, EastAuClimate,
       type = c("p", "a"), auto.key = list(lines = TRUE),
       ylab = "Days per year")
}
\keyword{datasets}
