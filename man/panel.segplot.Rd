\name{panel.segplot}
\alias{prepanel.segplot}
\alias{panel.segplot}
\title{ Default prepanel and panel functions for segplot  }
\description{
  Draws line segments or rectangles.  Mainly intended to be used in
  conjunction with the \code{segplot} function.
}
\usage{
prepanel.segplot(x, y, z, subscripts, horizontal = TRUE, ...)

panel.segplot(x, y, z, level = NULL, subscripts, 
              at,
              draw.bands = is.factor(z),
              col, alpha, 
              lty, lwd,
              border,
              col.symbol = col, alpha.symbol = alpha,
              col.regions = regions$col,
              band.height = 0.6,
              horizontal = TRUE, 
              ...,
              segments.fun = panel.segments,
              centers = NULL,
              pch = 16)
}

\arguments{
  \item{x, y, z}{ Vectors corresponding to \code{x1}, \code{x2} and
    \code{y} respectively in the \code{segplot} formula.  The names are
    different for compatibility with \code{panel.levelplot}.  These are
    all the original vectors in \code{data}, not subsetted for
    particular panels.  }
  \item{level}{ optional vector controlling color of segments }
  \item{centers}{ optional vector of \sQuote{centers} of the segments.
    If specified, points will be plotted at these \code{y}-locations.  }
  \item{pch}{ plotting character used for \code{centers}. }
  \item{subscripts}{ integer subscript to be used as an indexing vector
    for \code{x}, \code{y}, \code{z} and \code{level}, giving the packet
    for the current panel. }
  \item{horizontal}{ logical, whether the segments are to be drawn
    horizontally (the default) or vertically.  This essentially swaps
    the role of the x- and y-axes in each panel. }
  \item{at}{ values of \code{level} where color code changes }
  \item{draw.bands}{ logical, whether to draw rectangles instead of
    lines }
  \item{col, alpha, lty, lwd, border}{ Graphical parameters for the
    segment.  Defaults to parameter settings for \code{"plot.line"} or
    \code{"plot.polygon"} for segments and rectangles respectively.
    \code{col} is overridden by \code{col.regions} if \code{level} is
    not null. }
  \item{col.symbol, alpha.symbol}{ Graphical parameters for the point if
    \code{centers} are plotted.  Defaults to the corresponding
    parameters for the segment.
  }
  \item{col.regions}{ vector of colors as in \code{\link{levelplot}} }
  \item{band.height}{ height of rectangles (applicable if
    \code{draw.bands} is \code{TRUE} }

  \item{\dots}{ Other arguments, passed on to \code{panel.rect} (when
    \code{draw.bands=TRUE}), \code{segments.fun} (otherwise),
    \code{panel.points} (if \code{centers} is not \code{NULL}), etc. as
    appropriate. }

  \item{segments.fun}{ function used to plot segments when
    \code{draw.bands} is \code{FALSE}.  The default is to use
    \code{\link{panel.segments}}, but \code{\link{panel.arrows}} is a
    useful alternative (arguments to \code{segments.fun} can be provided
    via the \code{\dots} argument, see example for
    \code{\link{segplot}}).}

}
\value{
  For \code{prepanel.segplot} a list with components \code{xlim} and
  \code{ylim}.
}

\author{ Deepayan Sarkar \email{deepayan.sarkar@r-project.org} }
\seealso{ \code{\link{segplot}} }
\keyword{ dplot }
