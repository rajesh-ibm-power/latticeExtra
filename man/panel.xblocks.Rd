\name{panel.xblocks}
\alias{panel.xblocks}
\alias{panel.xblocks.default}
\alias{panel.xblocks.ts}
\title{
  Plot contiguous blocks along x axis.
}
\description{
  Plot contiguous blocks along x axis. A typical use would be to
  highlight events or periods of missing data.
}
\usage{
panel.xblocks(x, ...)

\S3method{panel.xblocks}{default}(x, y, ..., height = unit(1, "npc"),
              block.y = unit(0, "npc"), vjust = 0,
              col = NULL, border = NA, name = "xblocks",
              last.step = median(diff(tail(x))))

\S3method{panel.xblocks}{ts}(x, y = NULL, ...)
}
\arguments{
  \item{x, y}{
    In the default method, \code{x} gives the ordinates along the x axis and
    must be in increasing order. \code{y} gives the colour values to plot as
    contiguous blocks. These may be character (or factor) values (colour
    names or hex codes), numbers (indexing \code{palette()}), or
    logicals (where \code{TRUE} is \code{palette(1)} and \code{FALSE} is
    transparent). Missing values in \code{y} are not plotted.

    See also \code{col}, below, which over-rides values of \code{y}.

    In the \code{ts} method, plot values against their \code{time()},
    unless \code{y} is specified, when it acts just like
    the default method.
  }
  \item{\dots}{
    In the default method, further arguments are graphical parameters
    passed on to \code{\link[grid]{gpar}}.
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
    \code{\link[grid]{grid.rect}}.
  }
  \item{col}{
    if \code{col} is specified, then all blocks are drawn in the given
    colour. That is, all values of \code{y} are treated the same except
    \code{NA} / \code{FALSE} / \code{""}.

    If multiple colours are specified they will cycle.
  }
  \item{border}{
    border colour.
  }
  \item{name}{
    a name for the grob (grid object).
  }
  \item{last.step}{
    width (in native units) of the final block. Defaults to the median of
    the last 5 time steps (assuming steps are regular).
  }
}
\details{
  Blocks are drawn forward in "time" from the specified x locations,
  up until the following value. Contiguous blocks are calculated by
  \code{\link{rle}}.
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
%  \code{\link{xyplot.ts}},
  \code{\link{panel.rect}},
  \code{\link[grid]{grid.rect}}
}
\examples{
## Example of highlighting peaks in a time series.
flow <- ts(filter(rlnorm(200, mean = 1), 0.8, method = "r"))

## using an explicit panel function
xyplot(flow, panel = function(x, y, ...) {
  panel.xblocks(x, y > mean(y), col = "lightgrey")
  panel.xyplot(x, y, ...)
})
## using layers; this is the `ts` method because `>` keeps it as ts.
xyplot(flow) +
  layer_(panel.xblocks(flow > mean(flow), col = "lightgrey"))

## Example of alternating colours, here showing calendar months
flowdates <- Sys.Date() + as.vector(time(flow))
xyplot(flow) +
  layer_(panel.xblocks(time(flow), months(flowdates),
         col = c("lightgrey", "#e6e6e6"), border = "darkgrey"))

## Example of highlighting gaps (NAs) in time series.
## set up example data
z <- ts(matrix(1:18-1, ncol = 3))
colnames(z) <- c("A","B","C")
z[3:4, "B"] <- NA
z[1, "C"] <- NA
z
## Convert to logical time series showing only gaps
zExists <- !is.na(z)
attributes(zExists) <- attributes(z)
zExists
xyplot(zExists, panel = panel.xblocks, outer = TRUE, layout = c(1,3))
## Convert to time series of colour names: red indicates missing.
zCol <- z
zCol[] <- ifelse(is.na(z), "red", "grey")
xyplot(zCol, panel = panel.xblocks, outer = TRUE, layout = c(1,3),
       scales = list(y = list(draw = FALSE)))

## Example of overlaying blocks from a different series.
## Are US presidential approval ratings linked to sunspot activity?
## Set block height, default justification is along the bottom.
xyplot(presidents) + layer(panel.xblocks(sunspot.year > 50, height = 3))
## Multiple colour values given in the 'y' argument.
sscols <- cut(sunspot.year, c(50,150,Inf), labels=c("yellow","orange"))
xyplot(presidents, lwd = 2) +
  layer_(panel.xblocks(time(sunspot.year), y = sscols))

}
\keyword{ dplot }
