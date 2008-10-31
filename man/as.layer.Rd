\name{as.layer}
\alias{as.layer}
\alias{as.layer.trellis}
\title{ Overplot data with different scales }
\description{
  Simplifies overlaying of plots with different scales.
  The overlaid plots include custom axes and may be drawn in a different
  style.

  \emph{Note:} drawing plots with multiple scales is often a bad idea
  as it can be misleading.
}
\usage{
as.layer(x, ...)

\S3method{as.layer}{trellis}(x, axes = c("x", "y"), opposite = TRUE,
         outside = FALSE, ...)
}
\arguments{
  \item{x}{ a trellis object. }
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
\seealso{ \code{\link{layer}}, \code{\link[lattice]{panel.axis}} }
\examples{
set.seed(1)
foo <- list(x = 1:100, y = cumsum(rnorm(100)))
obj1 <- xyplot(y ~ x, foo, type = "l")
obj2 <- xyplot(y^2 ~ x, foo, type = "l")

## simple case: no axes for the overlaid plot
obj1 + as.layer(obj2, style = 2, axes = NULL)

## draw y axis inside (opposite); remove original ticks from right
update(obj1, scales = list(tck = c(1,0))) +
    as.layer(obj2, style = 2, axes = "y")

## draw original axis in that series' style color
col1 <- trellis.par.get("superpose.line")$col[1]
obj1 <- update(obj1, scales = list(tck = c(1,0),
    y = list(col = col1, col.line = col1) ))

## draw y axis outside (opposite); need extra space
update(obj1, lattice.options = list(layout.widths =
    list(right.padding = list(x = 2)))) +
  as.layer(obj2, style = 2, axes = "y", outside = TRUE)

## or draw both series as layers, over a blank "dummy" plot
yAxPad <- list(layout.widths = list(
    axis.left = list(x = 2.5, units = "char"),
    axis.right = list(x = 2.5, units = "char")))
dummy <- update(obj1, panel = function(...) NULL,
    scales = list(y = list(draw = FALSE)),
    lattice.options = yAxPad)
dummy +
  as.layer(obj1, style = 1, axes = "y", out = TRUE, opp = FALSE) +
  as.layer(obj2, style = 2, axes = "y", out = TRUE)

## multi-panel example
## a variant of Figure 5.13 from Sarkar (2008)
## http://lmdvr.r-forge.r-project.org/figures/figures.html?chapter=05;figure=05_13
data(SeatacWeather)
temp <- xyplot(min.temp + max.temp ~ day | month,
               data = SeatacWeather, type = "l", layout = c(3, 1))
rain <- xyplot(precip ~ day | month, data = SeatacWeather, type = "h")

merged <- temp + as.layer(rain, axes = "y", outside = TRUE, style = 3)
merged <- update(merged, lattice.options = list(layout.widths =
    list(right.padding = list(x = 2))),
    scales = list(y = list(tck = c(1,0))))
merged

## setting free scales now only applies to the first object!
## (rain still has same scales in all panels)
update(merged, scales = "free")

## multi-panel example with free scales (axes on all panels)
## this plot is getting really messy now...
temp.f <- update(temp, scales = "free")
rain.f <- update(rain, scales = "free")
merged.f <- temp.f +
    as.layer(rain.f, axes = "y", outside = TRUE, style = 3)
update(merged.f, between = list(x = 2),
    lattice.options = list(layout.widths =
        list(right.padding = list(x = 2))))

## applying one panel layer to several panels of another object
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free") +
as.layer(levelplot(volcano), under = TRUE)
}
\keyword{ aplot }
