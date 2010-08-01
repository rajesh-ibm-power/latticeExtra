\name{horizonplot}
\alias{horizonplot}
\alias{horizonplot.default}
\alias{panel.horizonplot}
\alias{prepanel.horizonplot}
\title{ Plot many time series in parallel }

\description{
  Plot many time series in parallel by cutting the y range into segments
  and overplotting them with color representing the magnitude and
  direction of deviation.
}

\usage{
horizonplot(x, data, \dots)

\S3method{horizonplot}{default}(x, data = NULL, \dots,
    horizonscale = NA,
    origin = function(y) na.omit(y)[1],
    colorkey = FALSE, legend = NULL,
    panel = panel.horizonplot,
    prepanel = prepanel.horizonplot,
    col.regions =
    c("#B41414","#E03231","#F7A99C","#9FC8DC","#468CC8","#0165B3"),
    strip = FALSE, strip.left = TRUE,
    par.strip.text = list(cex = 0.6),
    colorkey.digits = 3,
    groups = NULL,
    default.scales =
      list(y = list(relation = "free", axs = "i", 
                    draw = FALSE, tick.number = 2)))

panel.horizonplot(x, y, ..., border = NA, col.regions =
    c("#B41414","#E03231","#F7A99C","#9FC8DC","#468CC8","#0165B3"),
    origin)

prepanel.horizonplot(x, y, ..., horizonscale = NA,
    origin = function(y) na.omit(y)[1])
}

\arguments{
  \item{x, y}{ Argument on which argument dispatch is carried
    out. Typically this will be a multivariate time series.
    In the panel and prepanel functions, these are the data
    coordinates. 
  }
  \item{data}{
    Not used (at least, not used by \code{xyplot.ts}).
  }
  \item{\dots}{
    further arguments.  Arguments to \code{xyplot} as
    well as to the default panel function \code{\link{panel.horizonplot}}
    can be supplied directly to \code{horizonplot}.
    In typical usage, the method of \code{xyplot} called will be
    \code{\link{xyplot.ts}}.
  }
  \item{horizonscale}{
    the scale of each color segment. There are 3 positive
    segments and 3 negative segments. If this is a given as a
    number then all panels will have comparable distances, though not
    necessarily the same actual values (similar in concept to
    \code{scales$relation = "sliced"}). If \code{NA}, as it is by
    default, then the scale is chosen in each panel to cover the range
    of the data (unless overridden by \code{ylim}); see Details.
  }
  \item{origin}{
    the baseline y value for the first (positive) segment
    (i.e. the value at which red changes to blue). This can be a
    number, which is then fixed across all panels, or it can be a
    function, which is evaluated with the \code{y} values in each
    panel. The default is the first non-missing y value in each
    panel. See the Details section.
  }
  \item{colorkey, legend}{
    if \code{colorkey = TRUE} a suitable color scale bar is constructed
    using the values of \code{origin} and \code{horizonscale}. Further
    options can be passed to \code{colorkey} in list form, as with
    \code{\link{levelplot}}. 
  }
  \item{panel}{ function to render the graphic given the data.  This is
    the function that actually implements the display.  }
  \item{prepanel}{ function determining range of the data rectangle from
    data to be used in a panel. }
  \item{col.regions}{
    color scale, with at least 6 colors. This should be a divergent
    color scale (typically with white as the central color).
  }
  \item{strip, strip.left}{
    by default strips are only drawn on the left, to save space.
  }
  \item{par.strip.text}{
    graphical parameters for the strip text; see
    \code{\link{xyplot}}. One notable argument here is
    \code{lines}, allowing multi-line text.
  }
  \item{colorkey.digits}{
    digits for rounding values in colorkey labels.
  }
  \item{default.scales}{
    sets default values of \code{scales}; leave this alone, pass
    \code{scales} instead.
  }
  \item{groups}{
    not applicable to this type of plot.
  }
  \item{border}{
    border color for the filled polygons, defaults to no border.
  }
}

\details{
  This function draws time series as filled areas, with modifications
  to effectively visualise many time series in parallel. Data that would
  be drawn off the top of each panel is redrawn from the bottom of the
  panel in a darker color. Values below the origin are inverted and
  drawn in the opposite color. There are up to three shades (typically
  in blue) for data above the baseline and up to three shades (typically
  in red) for data below the baseline. See the article referenced below
  for an introduction to Horizon plots.

  There are three different cases of using this function:
  \enumerate{
    \item \code{horizonscale} unspecified (default case): then each
     panel will have different scales, and the colors represent
     deviations from the origin up to the maximum deviation from the
     origin in that panel. If \code{origin} is specified then that will
     be constant across panels; otherwise it defaults to the initial
     value.
   \item \code{horizonscale} specified but \code{origin} unspecified:
     the origin defaults to the initial value in each panel, and colors
     represent deviations from it in steps of \code{horizonscale} (up to
     3 steps each way).
   \item both \code{horizonscale} and \code{origin} specified: each
     panel will have the same scales, and colors represent fixed ranges
     of values. 
  }

  In each of these cases the \code{colorkey} is labelled slightly
  differently (see examples).
}

\section{Warning}{
  Note that the y scale in each panel defines the actual origin and
  scale used. The \code{origin} and \code{horizonscale} arguments are
  only used in the \code{prepanel} function to choose an appropriate y
  scale. The \code{ylim} argument therefore over-rides 
  \code{origin} and \code{horizonscale}. This also implies that choices
  of \code{scales$y$relation} other than \code{"free"} may have
  unexpected effects, particularly \code{"sliced"}, as these change the
  y limits from those requested by the prepanel function.
}


\value{
  An object of class \code{"trellis"}. The
  \code{\link[=update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[=print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.
}

\references{
  Stephen Few (2008). Time on the Horizon.
  \emph{Visual Business Intelligence Newsletter}, June/July 2008
  \url{http://www.perceptualedge.com/articles/visual_business_intelligence/time_on_the_horizon.pdf}
}

\author{
  Felix Andrews \email{felix@nfrac.org}
}

\seealso{
  \code{\link{Lattice}},
  \code{\link{xyplot.ts}},
  \code{\link{panel.xyarea}}
}

\examples{
## generate a random time series object with 12 columns
set.seed(1)
dat <- ts(matrix(cumsum(rnorm(200 * 12)), ncol = 12))
colnames(dat) <- paste("series", LETTERS[1:12])

## show simple line plot first, for reference.
xyplot(dat, scales = list(y = "same"))

## these layers show scale and origin in each panel...
infolayers <-
  layer(panel.scaleArrow(x = 0.99, digits = 1, col = "grey",
                         srt = 90, cex = 0.7)) +
  layer(lim <- current.panel.limits(),
    panel.text(lim$x[1], lim$y[1], round(lim$y[1],1), font = 2,
        cex = 0.7, adj = c(-0.5,-0.5), col = "#9FC8DC"))

## Case 1: each panel has a different origin and scale:
## ('origin' default is the first data value in each series).
horizonplot(dat, layout = c(1,12), colorkey = TRUE) +
  infolayers
 
## Case 2: fixed scale but different origin (baseline):
## (similar in concept to scales = "sliced")
horizonplot(dat, layout = c(1,12), horizonscale = 10, colorkey = TRUE) +
  infolayers

## Case 3: fixed scale and constant origin (all same scales):
horizonplot(dat, layout = c(1,12), origin = 0, horizonscale = 10, colorkey = TRUE) +
  infolayers

## same effect using ylim (but colorkey does not know limits):
horizonplot(dat, layout = c(1,12), ylim = c(0, 10), colorkey = TRUE) +
  infolayers

## same scales with full coverage of color scale:
horizonplot(dat, layout = c(1,12), origin = 0,
            scales = list(y = list(relation = "same")),
            colorkey = TRUE, colorkey.digits = 1) +
  infolayers


## illustration of the cut points used in the following plot
xyplot(EuStockMarkets, scales = list(y = "same"),
  panel = function(x, y, ...) {
    col <-
    c("#B41414","#E03231","#F7A99C","#9FC8DC","#468CC8","#0165B3")
    for (i in c(-3:-1, 2:0)) {
      if (i >= 0)
        yi <- pmax(4000, pmin(y, 4000 + 1000 * (i+1)))
      if (i < 0)
        yi <- pmin(4000, pmax(y, 4000 + 1000 * i))
      panel.xyarea(x, yi, origin = 4000,
        col = col[i+4], border = NA)
    }
    panel.lines(x, y)
    panel.abline(h = 4000, lty = 2)
  })

## compare with previous plot
horizonplot(EuStockMarkets, strip.left = TRUE, 
            scales = list(y = list(relation = "same")),
            origin = 4000, horizonscale = 1000, colorkey = TRUE) +
  infolayers

## a cut-and-stack plot; use constant y scales!
horizonplot(sunspots, cut = list(n = 23, overlap = 0),
  scales = list(draw = FALSE, y = list(relation = "same")),
  origin = 100, colorkey = TRUE,
  strip.left = FALSE, layout = c(1,23)) +
layer(grid::grid.text(round(x[1]), x = 0, just = "left"))
}
\keyword{hplot}
\keyword{ts}
