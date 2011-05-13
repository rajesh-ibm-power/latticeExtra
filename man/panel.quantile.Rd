\name{panel.quantile}
\alias{panel.quantile}
\title{
  Plot a quantile regression line with standard error bounds.
}
\description{
  Plot a quantile regression line with standard error bounds, using the
  \pkg{quantreg} package. This is based on the
  \code{\link[ggplot2]{stat_quantile}} function from \pkg{ggplot2}.
}
\usage{
panel.quantile(x, y, form = y ~ x, method = "rq", ...,
    tau = 0.5, ci = FALSE, ci.type = "default", level = 0.95,
    n = 100, col = plot.line$col, col.se = col,
    lty = plot.line$lty, lwd = plot.line$lwd,
    alpha = plot.line$alpha, alpha.se = 0.25, border = NA,
    superpose = FALSE,
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
    the model is constructed (approximately) as
    \code{method(form, tau = tau, data = list(x=x, y=y), ...)}.
    See the Examples section for common choices.
  }
  \item{\dots}{
    further arguments passed on to the model function (\code{method}),
    typically \code{\link[quantreg]{rq}}.
  }
  \item{tau}{
    \var{p} values for the quantiles to estimate.

    Note: only one value for \code{tau} can be specified if
    estimating confidence intervals with \code{ci}.
  }
  \item{ci, ci.type, level}{
    estimate a confidence interval at level \code{level} using the
    method \code{ci.type}; see \code{\link[quantreg]{predict.rq}}.
  }
  \item{n}{
    number of equi-spaced points on which to evaluate the function.
  }
  \item{col, col.se, lty, lwd, alpha, alpha.se, border}{
    graphical parameters. \code{col} and \code{alpha} apply to the
    line(s), while \code{col.se} and \code{alpha.se}
    apply to the shaded \code{ci} region.
  }
  \item{superpose}{
    if \code{TRUE}, plot each quantile line (\code{tau}) in a
    different style (using \code{trellis.par.get("superpose.line")}).
  }
  \item{subscripts, group.number, group.value,
    type, col.line, col.symbol, fill,
    pch, cex, font, fontface, fontfamily}{
    ignored.
  }
}
\details{
  It is recommended to look at \code{vignette("rq",package="quantreg")}.
}
\author{
  Felix Andrews \email{felix@nfrac.org}

  Based on \code{\link[ggplot2]{stat_quantile}} by Hadley Wickham.
}
\seealso{
  \code{\link[quantreg]{rq}},
  \code{\link{panel.smoother}},
  \code{\link[ggplot2]{stat_quantile}}
}
\examples{
library("quantreg")

set.seed(1)
xy <- data.frame(x = runif(100), y = rt(100, df = 5))
xyplot(y ~ x, xy) +
  layer(panel.quantile(x, y, tau = c(.95, .5, .05)))

if (require("splines")) {
  xyplot(y ~ x, xy) +
    layer(panel.quantile(y ~ ns(x, 3), tau = 0.9))

  xyplot(y ~ x, xy) +
    layer(panel.quantile(y ~ ns(x, 3), tau = 0.9, ci = TRUE))
}

xyplot(y ~ x, xy) +
  layer(panel.quantile(x, y, tau = c(.5, .9, .1), superpose = TRUE))
update(trellis.last.object(),
  auto.key = list(text = paste(c(50,90,10), "\% quantile"),
                  points = FALSE, lines = TRUE))

## seems not to work...
#xyplot(y ~ x, xy) +
#  layer(panel.quantile(y ~ qss(x, lambda=1), method = "rqss"))
}
\keyword{ dplot }
