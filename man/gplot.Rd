\name{gplot}
\alias{gplot}
\alias{gplot.default}
\alias{gplotArgs}
\alias{gplotArgs<-}
\alias{gplotArgs.default}
\title{Grouped Display}
\description{
  Generic plotting function to produce grouped display using lattice
}
\usage{
gplot(x, ...)
gplotArgs(x, ...)
gplotArgs(x) <- value
\method{gplot}{default}(x, \dots)
\method{gplotArgs}{default}(x, \dots)
}
\arguments{
  \item{x}{ an arbitrary object (typically one with a suitable method) }
  \item{value}{ list }
  \item{\dots}{ extra arguments, passed on as appropriate  }
}
\details{
  later.

}

\value{
  \code{gplot} should produce an object of class ``trellis''. The
  `update' method can be used to update components of the object and the
  `print' method (usually called by default) will plot it on an
  appropriate plotting device.

  \code{gplotArgs} should return a list.

}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{Lattice}}
}
\keyword{dplot}
