\name{panel.qqmath.tails}
\alias{panel.qqmath.tails}
\title{
  Approximate distribution in qqmath but keep points on tails.
}
\description{
  Panel function for \code{\link{qqmath}} to reduce the number of points
  plotted by sampling along the specified distribution. The usual method
  for such sampling is to use the \code{f.value} argument to
  \code{\link{panel.qqmath}}. However, this panel function differs in
  two ways:
    (1) a specified number of data points are retained (not
    interpolated) on each tail of the distribution.
    (2) the sampling is evenly spaced along the specified
    distribution automatically (whereas \code{f.value = ppoints(100)} is
    evenly spaced along the uniform distribution only).


  \emph{
    This function is deprecated as of \pkg{lattice} 0.18-4 (available for
    R 2.11.0). Use the \code{tails.n} argument of
    \code{\link{panel.qqmath}} instead.
  }
  
}
\usage{
panel.qqmath.tails(x, f.value = NULL, distribution = qnorm,
                    groups = NULL, ..., approx.n = 100, tails.n = 10)
}
\arguments{
  \item{x, f.value, distribution, groups}{
    see \code{\link{panel.qqmath}}.
  }
  \item{\dots}{
    further arguments passed on to \code{\link{panel.xyplot}}.
  }
  \item{approx.n}{
    number of points to use in approximating the distribution.
    Points will be equally spaced in the distribution space.
  }
  \item{tails.n}{
    number of points to retain (untouched) at both the high and low tails.
  }
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
  \code{\link{panel.qqmath}} which should be used instead (as of \pkg{lattice} 0.18-4).
}
\examples{

## see ?panel.qqmath

}
\keyword{ dplot }
