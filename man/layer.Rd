\name{layer}
\alias{layer}
\alias{+.trellis}
\alias{print.layer}
\alias{layer1}
\alias{layer2}
\alias{layer3}
\alias{layer4}
\alias{layer5}
\alias{undoLayer}
\alias{flattenPanel}
\title{ Conveniently augment trellis objects }
\description{
  A convenience mechanism to add new layers to a trellis object,
  optionally using a new data source.
  This works by augmenting the panel function with arbitrary
  expressions, defined using the \code{layer()} function.
}
\usage{
layer(..., data = NULL, under = FALSE, layer = NULL)

\S3method{+}{trellis}(x, lay)

undoLayer(x)
flattenPanel(x)
}
\arguments{
  \item{\dots}{ expressions as they would appear in a panel function.
    These can refer to the panel function arguments
    (typically \code{x}, \code{y} and \code{subscripts}), or
    alternatively \code{...}, which represents all panel function arguments.
    In addition, \code{data} is used as the evaluation frame. }
  \item{data}{ an optional data source (\code{data.frame}, \code{environment}, etc:
    see \code{\link{eval}}). The value \code{TRUE} can be given, in
    which case the \code{data} argument from the lattice call
    is used. }
  \item{under}{ whether the layer should be drawn below the existing
    panel. }
  \item{layer}{ index of the layer, used only to set lattice graphical
    parameters. }
  \item{x}{ a trellis object. }
  \item{lay}{ a layer object. }
}
\details{
  The \code{layer} mechanism allows for incremental design of
  trellis displays. It is an alternative to
  editing the panel function, designed mainly for interactive use.

  This is an experimental feature and is likely to change.

  When the \code{data} argument is \code{TRUE}, an attempt is made to use
  the object passed as a \code{data} argument in the original lattice
  plot call.

  The \code{undoLayer} function will reverse the effect of the layer
  last added to a trellis object. It can be applied repeatedly.

  The \code{flattenPanel} function will construct a human-readable
  function incorporating code from all layers (and the original panel
  function). Note that this does not return a usable function, as it
  lacks the correct argument list and ignores any extra data sources
  that layers might use. It is intended be edited manually.
}
\value{
  a \code{layer} object is defined as a list of expression objects,
  each of which may have a \code{data} attribute. The result of "adding"
  a layer to a trellis object (\code{+.trellis}) is the updated trellis
  object. Then \code{undoLayer} returns the original trellis object.

}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{update.trellis}} }
\examples{

foo <- xyplot(ozone ~ wind, environmental)

## overlay reference lines
foo <- foo + layer(panel.abline(h=0)) +
             layer(panel.lmline(x, y))

## underlay a flat colour
foo <- foo + layer(panel.fill(grey(.9)), under=TRUE)
foo

## remove the colour
foo <- undoLayer(foo)
foo

## layers can access the panel function arguments
foo <- foo + layer(ok <- (y>100),
            panel.text(x[ok], y[ok], y[ok], pos=1))
foo

## see an outline of the complete panel function
flattenPanel(foo)

## layers with superposed styles
zoip <- xyplot(ozone ~ wind | equal.count(temperature, 2),
        data=environmental) +
   layer2(panel.loess(x,y,span=.5)) +
   layer3(panel.loess(x,y,span=1))

## using other variables from the original `data` object
## NOTE: need subscripts=TRUE in original call!
zoip <- xyplot(wind ~ temperature | equal.count(radiation, 2),
        data=environmental, subscripts=TRUE)
zoip + layer(panel.points(..., pch=19,
       col=grey(1 - ozone[subscripts] / max(ozone))), data=TRUE)

## example of a new data source
qua <- xyplot(lat ~ long | cut(depth, 2), quakes,
    aspect="iso", pch=".", cex=2)
qua
## add layer showing distance from Auckland
newdat <- with(quakes, expand.grid(
            gridlat=seq(min(lat),max(lat),length=60),
            gridlon=seq(min(long),max(long),length=60)))
newdat$dist <- with(newdat, sqrt((gridlat - -36.87)^2 +
                                 (gridlon - 174.75)^2))
qua + layer(panel.contourplot(gridlon, gridlat, dist,
            contour=TRUE, subscr=TRUE), data=newdat, under=TRUE)

}
\keyword{ aplot }
