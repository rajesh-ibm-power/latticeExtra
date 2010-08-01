\name{segplot}
\alias{segplot}
\alias{segplot.formula}
\title{ Plot segments using the Trellis framework }

\description{ This function can be used to systematically draw segments
  using a formula interface to produce Trellis displays using the
  lattice package.  Segments can be drawn either as lines or bars, and
  can be color coded by the value of a covariate, with a suitable
  legend.}

\usage{
segplot(x, data, \dots)

\S3method{segplot}{formula}(x, data,
        level = NULL, centers = NULL,
        prepanel = prepanel.segplot,
        panel = panel.segplot,
        xlab = NULL, ylab = NULL,
        horizontal = TRUE,
        \dots,
        at, cuts = 30, colorkey = !is.null(level))

}

\arguments{
  \item{x}{ Argument on which argument dispatch is carried out.  For the
    \code{"formula"} method, a formula of the form \code{y ~ x1 + x2}
    (with further conditioning variables appended if necessary).  The
    terms in the formula must all be vectors of the same length.  Each
    element causes a line segment or rectangle to be drawn, with the
    vertical location determined by \code{y} and horizontal endpoints
    determined by \code{x1} and \code{x2}.  }
  \item{data}{ An optional data frame, list or environment where
    variables in the formula, as well as \code{level}, will be
    evaluated.
  }
  \item{level}{ An optional covariate that determines color coding of
    the segments}
  \item{centers}{ optional vector of \sQuote{centers} of the segments.
    If specified, points will be plotted at these \code{y}-locations.  }
  \item{prepanel}{ function determining range of the data rectangle from
    data to be used in a panel. }
  \item{panel}{ function to render the graphic given the data.  This is
    the function that actually implements the display.  }
  \item{xlab, ylab}{ Labels for the axes.  By default both are
    missing. }
  \item{horizontal}{ logical, whether the segments are to be drawn
    horizontally (the default) or vertically.  This essentially swaps
    the role of the x- and y-axes in each panel. }
  \item{\dots}{ further arguments.  Arguments to \code{levelplot} as
    well as to the default panel function \code{\link{panel.segplot}}
    can be supplied directly to \code{segplot}. }
  \item{colorkey}{ logical indicating whether a legend showing
    association of segment colors to values of \code{level} should be
    shown, or a list to control details of such a color key.  See
    details below. }
  \item{at, cuts}{ \code{at} specifies the values of \code{level} where
    the color code changes.  If \code{at} is missing, it defaults to
    \code{cuts} equispaced locations spanning the range of \code{levels}
  }
}

\details{
  The \code{levelplot} function from the lattice package is used to
  internally to implement this function.  In particular, the colorkey
  mechanism is used as it is, and documentation for
  \code{\link{levelplot}} should be consulted to learn how to
  fine tune it.
}


\value{
  An object of class \code{"trellis"}. The
  \code{\link[=update.trellis]{update}} method can be used to
  update components of the object and the
  \code{\link[=print.trellis]{print}} method (usually called by
  default) will plot it on an appropriate plotting device.
}

% \references{ ~put references to the literature/web site here ~ }

\author{ Deepayan Sarkar \email{deepayan.sarkar@r-project.org} }

\note{ Currently only horizontal segments are supported.  Vertical
  segments can be obtained by modifying the prepanel and panel functions
  suitably. }

\seealso{
  \code{\link{Lattice}},
  \code{\link{levelplot}},
  \code{\link{xyplot}}
}


\examples{

segplot(factor(1:10) ~ rnorm(10) + rnorm(10), level = runif(10))

data(USCancerRates)

segplot(reorder(factor(county), rate.male) ~ LCL95.male + UCL95.male,
        data = subset(USCancerRates, state == "Washington"))

segplot(reorder(factor(county), rate.male) ~ LCL95.male + UCL95.male,
        data = subset(USCancerRates, state == "Washington"),
        draw.bands = FALSE, centers = rate.male)

segplot(reorder(factor(county), rate.male) ~ LCL95.male + UCL95.male,
        data = subset(USCancerRates, state == "Washington"),
        level = rate.female, col.regions = terrain.colors)

segplot(reorder(factor(county), rate.male) ~ LCL95.male + UCL95.male,
        data = subset(USCancerRates, state == "Washington"),
        draw.bands = FALSE, centers = rate.male, 
        segments.fun = panel.arrows, ends = "both", 
        angle = 90, length = 1, unit = "mm")

}
\keyword{hplot}
