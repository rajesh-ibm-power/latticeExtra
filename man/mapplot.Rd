\name{mapplot}
\alias{mapplot}
\alias{mapplot.formula}
\alias{panel.mapplot}
\alias{prepanel.mapplot}
\title{ Trellis displays on Maps a.k.a. Choropleth maps }
\description{
  Produces Trellis displays of numeric (and eventually categorical) data
  on a map.  This is largely meant as a demonstration, and users looking
  for serious map drawing capabilities should look elsewhere (see below).
}
\usage{

mapplot(x, data, \dots)

\S3method{mapplot}{formula}(x, data, map, outer = TRUE,
        prepanel = prepanel.mapplot,
        panel = panel.mapplot,
        aspect = "iso",
        legend = NULL,
        breaks, cuts = 30,
        colramp = colorRampPalette(brewer.pal(n = 11, name = "Spectral")),
        colorkey = TRUE,
        \dots)


prepanel.mapplot(x, y, map, \dots)
panel.mapplot(x, y, map, breaks, colramp, lwd = 0.5, \dots)


}

\arguments{
  \item{x, y}{ For \code{mapplot}, an object on which method dispatch is
    carried out.  For the formula method, a formula of the form \code{y
      ~ x}, with additional conditioning variables as desired.  The
    extended form of conditioning using \code{y ~ x1 + x2} etc. is also
    allowed.  The formula might be interpreted as in a dot plot, except
    that \code{y} is taken to be the names of geographical units in
    \code{map}.

    Suitable subsets (packets) of \code{x} and \code{y} are passed to
    the prepanel and panel functions.
  }
  \item{data}{ A data source where names in the formula are evaluated }
  \item{map}{ An object of class \code{"map"} (package \code{maps}),
    containing boundary information.  The names of the geographical
    units must match the \code{y} variable in the formula.  }

  The remaining arguments are standard lattice arguments, relvant here
  mostly because they have different defaults than usual:

  \item{outer}{ logical; how variables separated by \code{+} in the
    formula are interpreted.  It is not advisable to change the
    default.  }
  \item{prepanel, panel}{ the prepanel and panel functions }
  \item{aspect}{ aspect ratio}
  \item{breaks, cuts, colramp}{ controls conversion of numeric \code{x}
    values to a false color.  \code{colramp} should be
    a function that produces colors (such as \code{\link{cm.colors}}).
    If it is \code{NULL}, colors are taken from
    \code{trellis.par.get("regions")}. }
  \item{legend, colorkey}{ controls legends; usually just a color key
    giving the association between numeric values of \code{x} and
    color.  }
  \item{lwd}{line width} 
  \item{\dots}{ Further arguments passed on to the underlying engine.
    See \code{\link{xyplot}} for details.  }
}
\value{
  An object of class \code{"trellis"}.
}
\references{ \url{http://en.wikipedia.org/wiki/Choropleth_map} }
\author{ Deepayan Sarkar }
\note{

  This function is meant to demonstrate how maps can be incorporated in
  a Trellis display.  Users seriously interested in geographical data
  should consider using software written by people who know what they
  are doing.

}
\seealso{ \code{\link{Lattice}}  }
\examples{

library(maps)
library(mapproj)


data(USCancerRates)

\dontshow{ .featured_example <- "USCancerRates" }
mapplot(rownames(USCancerRates) ~ log(rate.male) + log(rate.female),
        data = USCancerRates,
        map = map("county", plot = FALSE, fill = TRUE,
                  projection = "mercator"))

mapplot(rownames(USCancerRates) ~ log(rate.male) + log(rate.female),
        data = USCancerRates,
        map = map("county", plot = FALSE, fill = TRUE,
                  projection = "tetra"),
        scales = list(draw = FALSE))


data(ancestry)

county.map <- 
    map('county', plot = FALSE, fill = TRUE, 
        projection = "azequalarea")

\dontshow{ .featured_example <- "mapplot" }
## set a sequential color palette as current theme, and use it
opar <- trellis.par.get()
trellis.par.set(custom.theme(region = brewer.pal(9, "Purples"),
                             alpha.line = 0.5))
mapplot(county ~ log10(population), ancestry, map = county.map,
    colramp = NULL)
trellis.par.set(opar)

\dontrun{

## this may take a while (should get better area records)

county.areas <- 
    area.map(county.map, regions = county.map$names, sqmi = FALSE)
    
ancestry$density <- 
    with(ancestry, population / county.areas[as.character(county)])

mapplot(county ~ log(density), ancestry,
        map = county.map, border = NA,
        colramp = colorRampPalette(c("white", "black")))

}

}
\keyword{ hplot }
