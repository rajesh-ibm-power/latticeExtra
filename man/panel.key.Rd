\name{panel.key}
\alias{panel.key}
\title{
  Draw a simple key inside a panel of a lattice plot.
}
\description{
  Draw a \code{\link{simpleKey}} inside a panel of a lattice plot.
}
\usage{
panel.key(text, ..., corner = c(0, 1), x = corner[1], y = corner[2])
}
\arguments{
  \item{text, \dots}{
    entries in the \code{\link{simpleKey}}.
  }
  \item{corner, x, y}{
    defines the position of the key within the panel viewport. These are
    given in normalised coordinates between 0 and 1. The two elements
    of \code{corner} specify the x and y positions respectively.
  }
}
\seealso{
  \code{\link{simpleKey}},
  \code{\link{draw.key}},
  \code{\link{xyplot}}
}
\examples{
xyplot(ozone ~ wind | equal.count(temperature, 2),
       data = environmental) +
   layer(panel.loess(x, y, span = 0.5), style = 1) +
   layer(panel.loess(x, y, span = 1.0), style = 2) +
   layer(panel.key(c("span = 0.5", "span = 1.0"), corner = c(1,.98),
                   lines = TRUE, points = FALSE), packets = 1)
}
\keyword{ aplot }
