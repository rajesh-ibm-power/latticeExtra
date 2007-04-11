\name{hexbinplot}
\alias{hexbinplot}
\alias{hexbinplot.formula}
\alias{panel.hexbinplot}
\alias{prepanel.hexbinplot}
\alias{hexlegendGrob}
\title{Trellis version of Hexbin Displays}
\description{

  Display of hexagonally binned data, as implemented in the
  \code{hexbin} packge, under the Trellis framework, with associated
  utilities.  \code{hexbinplot} is the high level generic function, with
  the \code{"formula"} method doing the actual work.
  \code{prepanel.hexbinplot} and \code{panel.hexbinplot} are associated
  prepanel and panel functions.  \code{hexlegendGrob} produces a
  suitable legend.

}
\usage{
hexbinplot(x, \dots)

\method{hexbinplot}{formula}(x, data = parent.frame(),
           prepanel = prepanel.hexbinplot,
           panel = panel.hexbinplot,
           aspect = "xy",
           trans = NULL,
           inv = NULL,
           colorkey = TRUE,
           \dots,
           maxcnt,
           legend,
           legend.width = TRUE)

prepanel.hexbinplot(x, y, \dots)

panel.hexbinplot(x, y,
                 xbins = 30,
                 xbnds = c("panel", "data"),
                 ybnds = c("panel", "data"),

                 .prelim, .cpl, .xlim, .ylim, .aspect.ratio,

                 type = character(0),
                 \dots,
                 check.erosion = FALSE)


hexlegendGrob(legend = 1.2,
              inner = legend / 5,
              cex.labels = 1,
              cex.title = 1.2,
              style = "colorscale",
              minarea = 0.05, maxarea = 0.8,
              mincnt = 1, maxcnt,
              trans = NULL, inv = NULL,
              colorcut = seq(0, 1, length = 17),
              density = NULL, border = NULL, pen = NULL,
              colramp = function(n) { LinGray(n,beg = 90,end = 15) },
              \dots,
              vp = NULL,
              draw = FALSE)


}
\arguments{
  \item{x}{ For \code{hexbinplot}, the object on which method dispatch
    is carried out.
    
    For the \code{"formula"} methods, a formula describing the form of
    conditioning plot.  Formulas that are valid for \code{xyplot} are
    acceptable.

    In \code{panel.hexbinplot}, the x variable.
  }
  \item{y}{ In \code{panel.hexbinplot}, the y variable. } 

  \item{data}{For the \code{formula} method, a data frame containing
    values for any variables in the formula, as well as \code{groups}
    and \code{subset} if applicable (\code{groups} is ignored by the
    default panel function).  By default the environment where the
    function was called from is used.  }

  \item{xbins}{ number of hexagons covering x values.  The number of
    y-bins depends on this, the aspect ratio, and \code{xbnds} and
    \code{ybnds} }

  \item{xbnds, ybnds}{ Numeric vector specifying range of values that
    should be covered by the binning.  In a multi-panel display, it is
    not necessarily a good idea to use the same bounds (which along with
    \code{xbins} and the aspect ratio determine the size of the
    hexagons) for all panels.  For example, when data is concentrated in
    small subregions of different panels, more detail will be shown by
    using smaller hexagons covering those regions.  To control this,
    \code{xbnds} and \code{ybnds} can also be character strings
    \code{"panel"} or \code{"data"} (which are not very good names and
    may be changed in future).  In the first case, the bounds are taken
    to be the limits of the panel, in the second case, the limits of the
    data (packet) in that panel.  Note that all panels will have the
    same limits (enough to cover all the data) by default if
    \code{relation="free"} in the standard trellis argument
    \code{scales}, but not otherwise.
  }
  
  \item{minarea, maxarea, mincnt, maxcnt, trans, inv, colorcut, density,
    border, pen, colramp, style}{ see
    \code{\link[hexbin:gplot.hexbin]{gplot.hexbin}} }

  \item{prepanel, panel, aspect}{ See
    \code{\link[lattice:xyplot]{xyplot}}.  \code{aspect="fill"} is not
    allowed.  The current default of \code{"xy"} may not always be the
    best choice, often \code{aspect=1} will be more reasonable.  }
  
  \item{.prelim, .cpl, .xlim, .ylim, .aspect.ratio}{ for internal use }

  \item{colorkey}{logical, whether a legend should be drawn.  Currently
    a legend can be drawn only on the right. }

  \item{legend.width, legend}{ width of the legend in inches when
    \code{style} is \code{"nested.lattice"} or
    \code{"nested.centroids"}.  The name \code{legend.width} is used to
    avoid conflict with the standard trellis argument \code{legend}.  It
    is possible to specify additional legends using the \code{legend} or
    \code{key} arguments as long as they do not conflict with the
    hexbin legend (i.e., are not on the right).  }

  \item{inner}{ Inner radius in inches of hexagons in the legend when
    \code{style} is \code{"nested.lattice"} or
    \code{"nested.centroids"}.  }

  \item{cex.labels, cex.title}{ in the legend, multiplier for numeric
    labels and text annotation respectively }

  \item{type}{ character vector controlling additional augmentation of
    the display.  A \code{"g"} in \code{type} adds a reference grid,
    \code{"r"} adds a regression line (y on x), \code{"smooth"} adds a
    loess smooth }

  \item{check.erosion}{ ignored. Should not be specified. }
  
  \item{draw}{ logical, whether to draw the legend grob.  Useful when
    \code{hexlegendGrob} is used separately  }
  \item{vp}{ grid viewport to draw the legend in }

  \item{\dots}{ extra arguments, passed on as appropriate.  Arguments to
    \code{\link[hexbin:gplot.hexbin]{gplot.hexbin}},
    \code{\link[lattice:xyplot]{xyplot}}, \code{panel.hexbinplot} and
    \code{hexlegendGrob} can be supplied to the high level
    \code{hexbinplot} call.
  }


}
\details{

  The panel function \code{panel.hexbinplot} creates a hexbin object
  from data supplied to it and plots it using
  \code{\link[hexbin:grid.hexagons]{grid.hexagons}}.  To make panels
  comparable, all panels have the same \code{maxcnt} value, by default
  the maximum count over all panels.  This default value can be
  calculated only if the aspect ratio is known, and so
  \code{aspect="fill"} is not allowed.  The default choice of aspect
  ratio is different from the choice in \code{hexbin} (namely,
  \code{1}), which may sometimes give better results for multi-panel
  displays.  \code{xbnds} and \code{ybnds} can be numeric range vectors
  as in \code{hexbin}, but they can also be character strings specifying
  whether all panels should have the same bins.  If they are not, then
  bins in different panels could be of different sizes, in which case
  \code{style="lattice"} and \code{style="centroids"} should be
  interpreted carefully.


  The dimensions of the legend and the size of the hexagons therein are
  given in absolute units (inches) by \code{legend.width} and
  \code{inner} only when \code{style} is \code{"nested.lattice"} or
  \code{"nested.centroids"}.  For other styles, the dimensions of the
  legend are determined relative to the plot.  Specifically, the height
  of the legend is the same as the height of the plot (the panel and
  strip regions combined), and the width is the minimum required to fit
  the legend in the display.  This is different in some ways from the
  \code{hexbin} implementation.  In particular, the size of the hexagons
  in the legend are completely unrelated to the sizes in the panels,
  which is pretty much unavoidable because the sizes need not be the
  same across panels if \code{xbnds} or \code{ybnds} is \code{"data"}.
  The size of the hexagons encode information when \code{style} is
  \code{"lattice"} or \code{"centroids"}, consequently a warning is
  issued when a legend is drawn with wither of these styles.

}

\value{
  \code{hexbinplot} produces an object of class \code{"trellis"}. The
  code{update} method can be used to update components of the object and
  the \code{print} method (usually called by default) will plot it on an
  appropriate plotting device.  \code{hexlegendGrob} produces a
  \code{"grob"} (grid object).

}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{hexbin}}, \code{\link{xyplot}}
}

\examples{
mixdata <-
    data.frame(x = c(rnorm(5000),rnorm(5000,4,1.5)),
               y = c(rnorm(5000),rnorm(5000,2,3)),
               a = gl(2, 5000))
hexbinplot(y ~ x, mixdata, aspect = 1,
           trans = sqrt, inv = function(x) x^2)
hexbinplot(y ~ x | a, mixdata)
hexbinplot(y ~ x | a, mixdata, style = "lattice",
           xbnds = "data", ybnds = "data")
hexbinplot(y ~ x | a, mixdata, style = "nested.centroids")
hexbinplot(y ~ x | a, mixdata, style = "nested.centroids",
           border = FALSE, type = c("g", "smooth"))
}

\keyword{dplot}
