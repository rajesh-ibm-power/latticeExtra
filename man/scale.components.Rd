\name{scale.components}
\alias{scale.components}
\alias{xscale.components.logpower}
\alias{yscale.components.logpower}
\alias{xscale.components.fractions}
\alias{yscale.components.fractions}
\alias{xscale.components.log10ticks}
\alias{yscale.components.log10ticks}
\alias{xscale.components.log10.3}
\alias{yscale.components.log10.3}
\alias{xscale.components.subticks}
\alias{yscale.components.subticks}

\title{ Custom lattice axis scales }
\description{
  Convenience functions for drawing axes with various non-default tick
  positions and labels. 
}
\usage{
xscale.components.logpower(lim, ...)
yscale.components.logpower(lim, ...)

xscale.components.fractions(lim, logsc = FALSE, ...)
yscale.components.fractions(lim, logsc = FALSE, ...)

xscale.components.log10ticks(lim, logsc = FALSE, at = NULL, ...)
yscale.components.log10ticks(lim, logsc = FALSE, at = NULL, ...)

xscale.components.log10.3(lim, logsc = FALSE, at = NULL, ...)
yscale.components.log10.3(lim, logsc = FALSE, at = NULL, ...)

xscale.components.subticks(lim, ..., n = 5, n2 = n * 5, min.n2 = n + 5)
yscale.components.subticks(lim, ..., n = 5, n2 = n * 5, min.n2 = n + 5)
}
\arguments{
  \item{lim}{ scale limits. }
  \item{\dots}{
    passed on to \code{\link{xscale.components.default}} or
    \code{yscale.components.default}. 
  }
  \item{logsc}{
    \code{log} base, typically specified in the \code{scales}
    argument to a high-level lattice plot.
  }
  \item{at}{
    this is ignored unless it is \code{NULL}, in which case nothing is
    drawn. 
  }
  \item{n}{
    desired number of intervals between major axis ticks (passed to
    \code{\link{pretty}}.
  }
  \item{n2, min.n2}{
    desired, and minimum, number of intervals been minor axis ticks
    (passed to \code{\link{pretty}}.
  }
}
\details{
  These functions are intended to be passed to the
  \code{xscale.components} or \code{yscale.components} arguments of
  high-level lattice plots. See
  \code{\link{xscale.components.default}}. 
}
\references{
  Sarkar, Deepayan (2008) \dQuote{Lattice: Multivariate Data
    Visualization with R}, Springer.  ISBN: 978-0-387-75968-5
  \url{http://lmdvr.r-forge.r-project.org/figures/figures.html?chapter=08;figure=08_04}
}
\examples{
xyplot((1:200)/20 ~ (1:200)/20, type = c("p", "g"),
  scales = list(x = list(log = 2), y = list(log = 10)),
  xscale.components = xscale.components.fractions,
  yscale.components = yscale.components.log10ticks)

xyplot((1:200)/20 ~ (1:200)/20, type = c("p", "g"),
  scales = list(x = list(log = 2), y = list(log = 10)),
  xscale.components = xscale.components.logpower,
  yscale.components = yscale.components.log10.3)

dd <- as.Date("2000-01-01") + 0:365
xyplot(0:365 ~ dd, type = c("p", "g"),
  xscale.components = xscale.components.subticks,
  yscale.components = yscale.components.subticks)
}
\keyword{dplot}
