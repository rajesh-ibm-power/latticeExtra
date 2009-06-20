\name{as.layer}
\alias{as.layer}
\alias{as.layer.trellis}
\title{ Overlay panels with different scales }
\description{
  Allows overlaying of lattice plots with different scales.
  The overlaid plots include custom axes and may be drawn in a different
  style.

  \bold{Note:} in most cases you should use \code{\link{doubleYScale}},
  and NOT this function.
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
\seealso{
  \code{\link{doubleYScale}},
  \code{\link{layer}},
  \code{\link[lattice]{panel.axis}}
}
\examples{
set.seed(1)
foo <- list(x = as.Date(1:100, "2000-01-01"),
            y = cumsum(rnorm(100)))
obj1 <- xyplot(y ~ x, foo, type = "l",
  scales = list(format = "\%Y \%b"))
obj2 <- xyplot(jitter(y, amount = 3) ~ I(x + 360), foo,
  type = "l", scales = list(format = "\%Y \%b"))

## simple case: no axes for the overlaid plot
obj1 + as.layer(obj2, style = 2, axes = NULL)

## draw x axis inside (opposite); remove original ticks from top
update(obj1, scales = list(tck = c(1,0))) +
    as.layer(obj2, style = 2, axes = "x")

## draw original axis in that series' style color
col1 <- trellis.par.get("superpose.line")$col[1]
obj1 <- update(obj1, scales = list(tck = c(1,0),
    x = list(col = col1, col.line = col1) ))

## draw x axis outside (opposite); need extra space
update(obj1, lattice.options = list(layout.heights =
    list(top.padding = list(x = 2)))) +
  as.layer(obj2, style = 2, axes = "x", outside = TRUE)

## or draw both series as layers, over a blank "dummy" plot
xAxPad <- list(layout.heights = list(
    axis.bottom = list(x = 2.5, units = "char"),
    axis.top = list(x = 2.5, units = "char")))
dummy <- update(obj1, panel = function(...) NULL,
    scales = list(x = list(draw = FALSE)),
    lattice.options = xAxPad)
dummy +
  as.layer(obj1, style = 1, axes = "x", out = TRUE, opp = FALSE) +
  as.layer(obj2, style = 2, axes = "x", out = TRUE)

## applying one panel layer to several panels of another object
xyplot(Sepal.Length + Sepal.Width ~ Petal.Length + Petal.Width | Species,
       data = iris, scales = "free") +
   as.layer(levelplot(volcano), under = TRUE)
}
\keyword{ aplot }
