\name{panel.2dsmoother}
\alias{panel.2dsmoother}
\title{
  Plot a smooth approximation of z over x and y.
}
\description{
  Plot a smooth approximation, using \code{\link{loess}} by default, of
  one variable (\code{z}) against two others (\code{x} and \code{y}).

  This panel function should be used with a \code{\link{levelplot}}.
}
\usage{
panel.2dsmoother(x, y, z, subscripts = TRUE,
    form = z ~ x * y, method = "loess", ...,
    args = list(), n = 100)
}
\arguments{
  \item{x, y, z}{
    data points. If these are missing, they will be looked for in the
    environment of \code{form}. So in many cases you can skip these if
    passing \code{form}. In fact, for convenience, the formula
    can be passed as the first argument (i.e. \code{x}).
  }
  \item{form, method}{
    the smoothing model is constructed (approximately) as
    \code{method(form, data = list(x=x, y=y, z=z), {args})}.

    See the Examples section for common choices.
  }
  \item{subscripts}{
    data indices for the current packet, as passed in by \code{levelplot}.
  }
  \item{\dots}{
    further arguments passed on to \code{\link{panel.levelplot}}.
  }
  \item{args}{
    a list of further arguments to the model function (\code{method}).
  }
  \item{n}{
    number of equi-spaced points along each of x and y on which to evaluate the smooth function.
  }
}
\details{
  This should work with any model function that takes a formula
  argument, and has a \code{predict} method argument.
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
  \code{\link{loess}},
  \code{\link{panel.smoother}}
}
\examples{
set.seed(1)
xyz <- data.frame(x = rnorm(100), y = rnorm(100))
xyz$z <- with(xyz, x * y + rnorm(100, sd = 1))

levelplot(z ~ x * y, xyz, panel = panel.2dsmoother)

## showing data points on the same color scale
levelplot(z ~ x * y, xyz,
          panel = panel.levelplot.points, cex = 1.2) +
  layer_(panel.2dsmoother(..., n = 200))

## simple linear regression model
levelplot(z ~ x * y, xyz,
          panel = panel.levelplot.points) +
  layer_(panel.2dsmoother(..., method = "lm"))

## GAM smoother with smoothness by cross validation
if (require("mgcv"))
  levelplot(z ~ x * y, xyz, panel = panel.2dsmoother,
            form = z ~ s(x) + s(y), method = "gam")
}
\keyword{ dplot }
