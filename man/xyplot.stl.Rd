\name{xyplot.stl}
\alias{xyplot.stl}
\title{ Display stl fits with Lattice }

\description{
  Display \code{\link{stl}} decomposition (seasonal, trend and irregular
  components using loess) with Lattice, like the base graphics function
  \code{\link{plot.stl}}. 
}

\usage{
\method{xyplot}{stl}(x, data = NULL,
       outer = TRUE,
       layout = c(1, 4),
       strip = FALSE,
       strip.left = TRUE,
       as.table = TRUE,
       ylab = "",
       between = list(y = 0.5),
       panel =
       function(..., type) {
           if (packet.number() == 4) type <- "h"
           panel.xyplot(..., type = type)
       },
       ...)
}

\arguments{
  \item{x}{ an \code{\link{stl}} object. }
  \item{data}{ ignored. }
  \item{outer, layout, strip, strip.left}{
    passed to \code{\link{xyplot.ts}}.
  }
  \item{as.table, ylab, between, panel, ...}{
    passed to \code{\link{xyplot.ts}}.
  }
}

\details{
  Unless \code{strip.left} is passed in explicitly, a custom strip will
  be drawn, where shaded bars are comparable across panels (representing
  the same data range).
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

\seealso{
  \code{\link{stl}},
  \code{\link{xyplot.ts}},
  \code{\link{xyplot}}
}


\examples{
## example from ?stl
xyplot(stl(log(co2), s.window=21),
       main = "STL decomposition of CO2 data")
}
\keyword{hplot}
\keyword{ts}