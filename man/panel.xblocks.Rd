\name{panel.xblocks}
\alias{panel.xblocks}
\alias{panel.xblocks.default}
\alias{panel.xblocks.ts}
\alias{panel.xblocks.zoo}
\title{
  Plot contiguous blocks along x axis.
}
\description{
  Plot contiguous blocks along x axis. A typical use would be to
  highlight events or periods of missing data.
}
\usage{
panel.xblocks(x, ...)

\method{panel.xblocks}{default}(x, y, ..., col = NULL, border = NA, 
              height = unit(1, "npc"),
              block.y = unit(0, "npc"), vjust = 0,
              name = "xblocks", gaps = FALSE,
              last.step = median(diff(tail(x))))

\method{panel.xblocks}{ts}(x, y = x, ...)
\method{panel.xblocks}{zoo}(x, y = x, ...)
}
\arguments{
  \item{x, y}{
    In the default method, \code{x} gives the ordinates along the x axis and
    must be in increasing order. \code{y} gives the color values to plot as
    contiguous blocks. If \code{y} is numeric, data coverage is plotted,
    by converting it into a logical (\code{!is.na(y)}). Finally, if
    \code{y} is a function, it is applied to \code{x} (\code{time(x)} in
    the time series methods). 
    
    If \code{y} has character (or factor) values, these are interpreted
    as colors -- and should therefore be color names or hex
    codes. Missing values in \code{y} are not plotted.  The default
    color is taken from the current theme:
    \code{trellis.par.get("plot.line")$col}. If \code{col} is given,
    this over-rides the block colors.

    The \code{ts} and \code{zoo} methods plot the \code{y} values
    against the time index \code{time(x)}. 
  }
  \item{\dots}{
    In the default method, further arguments are graphical parameters
    passed on to \code{\link{gpar}}.
  }
  \item{col}{
    if \code{col} is specified, it determines the colors of the blocks
    defined by \code{y}. If multiple colors are specified they will be
    repeated to cover the total number of blocks.
  }
  \item{border}{
    border color.
  }
  \item{height}{
    height of blocks, defaulting to the full panel height. Numeric
    values are interpreted as native units.
  }
  \item{block.y}{
    y axis position of the blocks. Numeric values are interpreted as
    native units.
  }
  \item{vjust}{
    vertical justification of the blocks relative to \code{block.y}. See
    \code{\link{grid.rect}}.
  }
  \item{name}{
    a name for the grob (\pkg{grid} object).
  }
  \item{gaps}{
    Deprecated.
    Use \code{panel.xblocks(time(z), is.na(z))} instead.
  }
  \item{last.step}{
    width (in native units) of the final block. Defaults to the median of
    the last 5 time steps (assuming steps are regular).
  }
}
\details{
  Blocks are drawn forward in "time" from the specified x locations,
  up until the following value. Contiguous blocks are calculated using
  \code{\link{rle}}.
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
  \code{\link{xyplot.ts}},
  \code{\link{panel.rect}},
  \code{\link{grid.rect}}
}
\examples{
## Example of highlighting peaks in a time series.
set.seed(0)
flow <- ts(filter(rlnorm(200, mean = 1), 0.8, method = "r"))

## using an explicit panel function
xyplot(flow, panel = function(x, y, ...) {
  panel.xblocks(x, y > mean(y), col = "lightgray")
  panel.xyplot(x, y, ...)
})
## using layers; this is the `ts` method because `>` keeps it as ts.
xyplot(flow) +
  layer_(panel.xblocks(flow > mean(flow), col = "lightgray"))

\dontshow{ .featured_example <- TRUE }
## Example of alternating colors, here showing calendar months
flowdates <- as.Date("2000-01-01") + as.numeric(time(flow))
xyplot(flow ~ flowdates, type = "l") +
  layer_(panel.xblocks(x, months,
         col = c("lightgray", "#e6e6e6"), border = "darkgray"))

## highlight values above and below thresholds.
## blue, gray, red colors:
rgb <- hcl(c(0, 0, 260), c = c(100, 0, 100), l = c(90, 90, 90))
xyplot(flow) +
  layer_(panel.xblocks(time(flow),
         cut(flow, c(0,15,30,Inf), labels = rgb)))

## Example of highlighting gaps (NAs) in time series.
## set up example data
z <- ts(cbind(A = 0:5, B = c(6:7, NA, NA, 10:11), C = c(NA, 13:17)))

## show data coverage only (highlighting gaps)
xyplot(z, panel = panel.xblocks,
       scales = list(y = list(draw = FALSE)))

## draw gaps in darkgray
xyplot(z, type = c("p","s")) +
  layer_(panel.xblocks(x, is.na(y), col = "darkgray"))

## Example of overlaying blocks from a different series.
## Are US presidential approval ratings linked to sunspot activity?
## Set block height, default justification is along the bottom.
xyplot(presidents) + layer(panel.xblocks(sunspot.year > 50, height = 2))
}
\keyword{ dplot }
