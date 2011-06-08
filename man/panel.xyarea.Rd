\name{panel.xyarea}
\alias{panel.xyarea}
\alias{panel.xyarea.default}
\alias{panel.xyarea.ts}
\alias{panel.xyarea.zoo}
\alias{panel.qqmath.xyarea}
\title{
  Plot series as filled polygons.
}
\description{
  Plot series as filled polygons connected at given origin level (on y axis).
}
\usage{
panel.xyarea(x, ...)

\method{panel.xyarea}{default}(x, y, groups = NULL, origin = NULL, horizontal = FALSE,
             col, col.line, border, lty, lwd, alpha, ...,
             fill, panel.groups = panel.xyarea)

\method{panel.xyarea}{ts}(x, y = x, ...)
\method{panel.xyarea}{zoo}(x, y = x, ...)

panel.qqmath.xyarea(x, y = NULL, f.value = NULL, distribution = qnorm,
                    qtype = 7, groups = NULL, ..., tails.n = 0)
}
\arguments{
  \item{x, y}{
    data vectors.
  }
  \item{groups}{
    a factor defining groups.
  }
  \item{origin}{
    level on y axis to connect the start and end of the series.
    If \code{NULL}, the polygon is filled to the bottom of the panel.
    It is flipped if \code{horizontal = TRUE}.
  }
  \item{horizontal}{
    if this is set to \code{TRUE}, then the origin is a level on the x
    axis, rather than the default which is on the y axis. This is the
    opposite of what you might expect, but is for consistency with
    \code{panel.xyplot}. 
  }
  \item{col, col.line, border, lty, lwd, alpha}{
    graphical parameters taken from
    \code{trellis.par.get("plot.polygon")} or
    \code{trellis.par.get("superpose.polygon")} (when groups defined).
    \code{col.line} overrides \code{col}.
  }
  \item{\dots}{
    further arguments passed on to \code{\link{panel.polygon}}.
    For \code{panel.qqmath.xyarea}, passed to \code{panel.xyarea}.
  }
  \item{fill}{
    ignored; use \code{col} instead.
  }
  \item{panel.groups}{
    used in \code{\link{panel.superpose}}.
  }
  \item{f.value, distribution, qtype, tails.n}{
    as in \code{\link{panel.qqmath}}.
  }
}
\details{
  none yet.
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
  \code{\link{panel.xyplot}},
  \code{\link{panel.polygon}}
}
\examples{
xyplot(sunspot.year, panel = panel.xyarea, origin = 0,
  aspect = "xy", cut = list(n = 3, overlap = 0))

## two series superposed: one filled, one as a line.
xyplot(ts.union(data = sunspot.year, lag10 = lag(sunspot.year, 10)),
  aspect = "xy", cut = list(n = 3, overlap = 0),
  superpose = TRUE,
  panel = panel.superpose,
  panel.groups = function(..., group.number) {
    if (group.number == 1)
      panel.xyarea(...) else panel.xyplot(...)
  }, border = NA, 
  par.settings = simpleTheme(col = c("grey", "black"), lwd = c(5,2)))

## missing values are handled by splitting the series
tmp <- window(sunspot.year, start = 1900)
tmp[c(1:2, 50:60)] <- NA
xyplot(tmp, panel = panel.xyarea, origin = 0)

set.seed(0)
qqmath(~ data, make.groups(baseline = rnorm(100),
                           other = rnorm(100) * 2 - 0.5),
  groups = which, distribution = qunif,
  panel = panel.qqmath.xyarea, xlim = 0:1,
  auto.key = list(points = FALSE, rectangles = TRUE),
  par.settings = simpleTheme(col = c("blue", "green"),
                             alpha = 0.5))
}
\keyword{ dplot }
