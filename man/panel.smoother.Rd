\name{panel.smoother}
\alias{panel.smoother}
\title{
  Plot a smoothing line with standard error bounds.
}
\description{
  Plot a smoothing line with standard error bounds.
  This is based on the \code{\link[ggplot2]{stat_smooth}}
  function from \pkg{ggplot2}.
}
\usage{
panel.smoother(x, y, form = y ~ x, method = "loess", ...,
    se = TRUE, level = 0.95, n = 100,
    col = plot.line$col, col.se = col,
    lty = plot.line$lty, lwd = plot.line$lwd,
    alpha = plot.line$alpha, alpha.se = 0.25, border = NA,
    ## ignored: ##
    subscripts, group.number, group.value,
    type, col.line, col.symbol, fill,
    pch, cex, font, fontface, fontfamily)
}
\arguments{
  \item{x, y}{
    data points. If these are missing, they will be looked for in the
    environment of \code{form}. So in many cases you can skip these if
    passing \code{form}. In fact, for convenience, the formula
    can be passed as the first argument (i.e. \code{x}).
  }
  \item{form, method}{
    the smoothing model is constructed (approximately) as
    \code{method(form, data = list(x=x, y=y), ...)}.
    See the Examples section for common choices.
  }
  \item{\dots}{
    further arguments passed on to the model function (\code{method}).
  }
  \item{se, level}{
    estimate standard errors on the smoother, at the given \code{level},
    and plot these as a band.
  }
  \item{n}{
    number of equi-spaced points on which to evaluate the smooth function.
  }
  \item{col, col.se, lty, lwd, alpha, alpha.se, border}{
    graphical parameters. \code{col} and \code{alpha} apply to the
    smoothing line, while \code{col.se} and \code{alpha.se}
    apply to the shaded \code{se} region.
  }
  \item{subscripts, group.number, group.value,
    type, col.line, col.symbol, fill,
    pch, cex, font, fontface, fontfamily}{
    ignored.
  }
}
\details{
  This should work with any model function that takes a formula
  argument, and has a \code{predict} method with a \code{se} argument.
}
\author{
  Felix Andrews \email{felix@nfrac.org}

  Based on \code{\link[ggplot2]{stat_smooth}} by Hadley Wickham.
}
\seealso{
  \code{\link{panel.loess}},
  \code{\link{panel.quantile}},
  \code{\link[ggplot2]{stat_smooth}}
}
\examples{
set.seed(1)
xy <- data.frame(x = runif(100),
                 y = rt(100, df = 5),
                 y2 = rt(100, df = 5) + 1)

xyplot(y ~ x, xy, panel = function(...) {
       panel.xyplot(...)
       panel.smoother(..., span = 0.9)
})

## per-group layers with glayer (pass `...` to get styles)
xyplot(y + y2 ~ x, xy) +
  glayer(panel.smoother(...))

## natural spline with 5 degrees of freedom
if (require("splines"))
  xyplot(y ~ x, xy) +
    layer(panel.smoother(y ~ ns(x,5), method = "lm"))

## thin plate regression spline with smoothness
## chosen by cross validation (see ?mgcv::gam)
if (require("mgcv"))
  xyplot(y ~ x, xy) +
    layer(panel.smoother(y ~ s(x), method = "gam"))

## simple linear regression with standard errors:
xyplot(y ~ x, xy) +
  layer(panel.smoother(x, y, method = "lm"), style = 2)
}
\keyword{ dplot }
