\name{as.layer}
\alias{as.layer}
\alias{as.layer.layer}
\alias{as.layer.trellis}
\title{ Overlay panels of Trellis plots on same or different scales }
\description{
  Allows overlaying of Trellis plots, drawn on the same scales or on
  different scales in each of the x and y dimensions.
  There are options for custom axes and graphical styles.
}
\usage{
as.layer(x, ...)

\S3method{as.layer}{trellis}(x, x.same = TRUE, y.same = TRUE,
         axes = c(if (!x.same) "x", if (!y.same) "y"), opposite = TRUE,
         outside = FALSE, theme = x$par.settings, ...)
}
\arguments{
  \item{x}{ a trellis object. }
  \item{x.same}{ retains the existing panel x scale for the new layer,
    rather than using the layer's native x scale. }
  \item{y.same}{ retains the existing panel y scale. }
  \item{axes}{ which of the axes to draw (\code{NULL} for neither).
    Axes might not be drawn anyway, such as if
    \code{scales$draw == FALSE}. }
  \item{opposite}{ whether to draw axes on the opposite side to normal:
         that is, the top and/or right sides rather than bottom and/or
         left. May be a vector of length 2 to specify for x and y axes
         separately. }
  \item{outside}{ whether to draw the axes outside the plot region. Note
         that space for outside axes will not be allocated
         automatically. May be a vector of length 2 to specify for x and y axes
         separately. }
  \item{theme}{ passed to \code{\link{layer}}. }
  \item{\dots}{ passed to \code{\link{layer}}: typically
    the \code{style} argument would be specified. }
}
\details{
  Panels from the trellis object \code{x} will be drawn in the
  corresponding panel of another trellis object, so packet numbers match
  (see examples).

  Axis setting are taken from the trellis object \code{x}, so most
  \code{scales} arguments such as \code{draw}, \code{at}, \code{labels} etc will
  carry over to the overlaid axes. Only the main axis settings are used
  (i.e. left or bottom), even when \code{opposite = TRUE}.

  Currently, outside top axes will be drawn in the strip if there are strips.
}
\value{
  an updated trellis object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{
  \code{\link{doubleYScale}},
  \code{\link{layer}},
  \code{\link{panel.axis}}
}
\examples{
b1 <- barley
b2 <- barley
b2$yield <- b2$yield + 10

## panels are matched up by packet number
dotplot(variety ~ yield | site * year, b1) +
  as.layer(dotplot(variety ~ yield | site * year, b2, col = "red"))

## which gives the same result as:
dotplot(variety ~ yield | site * year, data = b1, subscripts = TRUE) +
  layer(panel.dotplot(yield[subscripts], variety[subscripts], col = "red"),
        data = b2)


## example with all same scales (the default):
xyplot(fdeaths ~ mdeaths) +
  as.layer(xyplot(fdeaths ~ mdeaths, col = 2, subset = ldeaths > 2000))

## same x scales, different y scales:
xyplot(fdeaths ~ mdeaths) +
  as.layer(bwplot(~ mdeaths, box.ratio = 0.2), y.same = FALSE)

## same y scales, different x scales:
xyplot(fdeaths ~ mdeaths) +
  as.layer(bwplot(fdeaths ~ factor(mdeaths*0), box.ratio = 0.2), x.same = FALSE)

## as.layer() is called automatically if two plots are added:
histogram(~ ldeaths, type = "density") + densityplot(~ ldeaths, lwd = 3)

## applying one panel layer to several panels of another object
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free") +
   as.layer(levelplot(volcano), x.same = FALSE, y.same = FALSE, under = TRUE)
}
\keyword{ aplot }
