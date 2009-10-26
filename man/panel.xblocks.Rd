\name{panel.xblocks}
\alias{panel.xblocks}
\alias{panel.xblocks.default}
\alias{panel.xblocks.ts}
\title{
  Plot contiguous blocks along x axis.
}
\description{
  Plot contiguous blocks along x axis.
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
    if \code{col} is specified, then all values of \code{y} that are not
    \code{NA} or \code{FALSE} are drawn in this colour.
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
  \code{\link{panel.rect}},
  \code{\link[grid]{grid.rect}}
}
\examples{
## Are US presidential approval ratings linked to sunspot activity?

## 'ts' method; set block height, default justification is at bottom.
xyplot(presidents) + layer(panel.xblocks(sunspot.year > 50, height = 3))

## blocks in a light shading colour, full panel height.
xyplot(presidents) +
  layer(panel.xblocks(sunspot.year > 50, col = "#e6e6e6"), under=TRUE)

## multiple colour values given in the 'y' argument.
sscols <- cut(sunspot.year, c(50,150,Inf), labels=c("yellow","orange"))
xyplot(presidents, lwd = 2) +
  layer(panel.xblocks(time(sunspot.year), y = sscols, alpha = 0.5))
}
\keyword{ dplot }
