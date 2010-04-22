\name{panel.lmlineq}
\alias{panel.lmlineq}
\alias{panel.ablineq}
\title{
  Draw a line with a label, by default its equation
}
\description{
  This is an extension of the panel functions \code{\link{panel.abline}} and
  \code{\link{panel.lmline}} to also draw a label on the line. The
  default label is the line equation, and optionally the R squared value
  of its fit to the data points.
}
\usage{
panel.ablineq(a = NULL, b = 0,
              h = NULL, v = NULL,
              reg = NULL, coef = NULL,
              pos = if (rotate) 1 else NULL,
              offset = 0.5, adj = NULL,
              at = 0.5, x, y,
              rotate = FALSE, srt = 0,
              label = NULL,
              varNames = alist(y = y, x = x),
              varStyle = "italic",
              fontfamily = "serif",
              digits = 3,
              r.squared = FALSE, sep = ", ", sep.end = "",
              col, col.text, col.line,
              ..., reference = FALSE)

panel.lmlineq(x, y, ...)
}
\arguments{
  \item{a, b, h, v, reg, coef}{
    specification of the line.
    The simplest usage is to give \code{a} and \code{b} to describe the
    line \emph{y = a + b x}.
    Horizontal or vertical lines can be specified as
    arguments \code{h} or \code{v}, respectively.
    The first argument (\code{a}) can also be a model object produced by
    \code{\link{lm}}.
    See \code{\link{panel.abline}} for more details.
  }
  \item{pos, offset}{
    passed on to \code{\link{panel.text}}.
    For \code{pos}: 1 = below, 2 = left, 3 = above, 4 = right,
    and the \code{offset} (in character widths) is applied.
  }
  \item{adj}{
    passed on to \code{\link{panel.text}}.
    c(0,0) = above right, c(1,0) = above left,
    c(0,1) = below right, c(1,1) = below left;
    offset does not apply when using \code{adj}.
  }
  \item{fontfamily}{
    passed on to \code{\link{panel.text}}.
  }
  \item{at}{
    position of the equation as a fractional distance along the line.
    This should be in the range 0 to 1.
    When a vertical line is drawn, this gives the vertical position of
    the equation.
  }
  \item{x, y}{
    position of the equation in native units. If given, this over-rides
    \code{at}.
    For \code{panel.lmlineq} this is the data, passed on as \code{lm(y ~ x)}.
  }
  \item{rotate, srt}{
    set \code{rotate = TRUE} to align the equation with the line.
    This will over-ride \code{srt}, which otherwise gives the rotation
    angle.
    Note that the calculated angle depends on the current device size;
    this will be wrong if you change the device aspect ratio after plotting.
  }
  \item{label}{
    the text to draw along with the line. If specified, this will be
    used instead of an equation.
  }
  \item{varNames}{
    names to display for \code{x} and/or \code{y}.
    This should be a list
    like \code{list(y = "Q", x = "X")}
    or, for mathematical symbols,
    \code{alist(y = (alpha + beta), x = sqrt(x[t]))}.
  }
  \item{varStyle}{
    the name of a \code{\link{plotmath}} function to wrap around the
    equation expression, or \code{NULL}. E.g. \code{"bolditalic"},
    \code{"displaystyle"}.
  }
  \item{digits}{
    number of decimal places to show for coefficients in equation.
  }
  \item{r.squared}{
    the \eqn{R^2} statistic to display along with the equation of a line.
    This can be given directly as a number, or \code{TRUE}, in which
    case the function expects a model object (typically
    \code{\link{lm}}) and extracts the \eqn{R^2} statistic from it.
  }
  \item{sep, sep.end}{
    The \eqn{R^2} (\code{r.squared}) value is separated from the
    equation by the string \code{sep}, and also \code{sep.end} is added
    to the end. For example: 
    \code{panel.ablineq(lm(y ~ x), r.squared = TRUE,
      sep = " (", sep.end = ")")}.
  }
  \item{\dots, col, col.text, col.line}{
    passed on to \code{\link{panel.abline}} and
    \code{\link{panel.text}}.
    Note that \code{col} applies to both text and line; \code{col.text}
    applies to the equation only, and \code{col.line} applies to line only.
  }
  \item{reference}{
    whether to draw the line in a "reference line" style, like that used
    for grid lines.
  }
}
\details{
  The equation is constructed as an expression using \code{\link{plotmath}}.
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
  \code{\link{panel.abline}},
  \code{\link{panel.text}},
  \code{\link{lm}},
  \code{\link{plotmath}}
}
\examples{
set.seed(0)
xsim <- rnorm(50, mean = 3)
ysim <- (0 + 2 * xsim) * (1 + rnorm(50, sd = 0.3))

## basic use as a panel function
xyplot(ysim ~ xsim, panel = function(x, y, ...) {
  panel.xyplot(x, y, ...)
  panel.ablineq(a = 0, b = 2, adj = c(0,1))
  panel.lmlineq(x, y, adj = c(1,0), lty = 2,
                col.line = "grey", digits = 1)
})

## using layers:
xyplot(ysim^2 ~ xsim) +
  layer(panel.ablineq(lm(y ~ x, subset = x <= 3),
    varNames = alist(y = y^2, x = x[x <= 3]), pos = 4))

## rotated equation (depends on device aspect at plotting time)
xyplot(ysim ~ xsim) +
  layer(panel.ablineq(lm(y ~ x), rotate = TRUE, at = 0.8))

## horizontal and vertical lines
xyplot(ysim ~ xsim) +
  layer(panel.ablineq(v = 3, pos = 4, at = 0.1, lty = 2,
                      label = "3.0 (critical value)")) +
  layer(panel.ablineq(h = mean(ysim), pos = 3, at = 0.15, lty = 2,
                      varNames = alist(y = plain(mean)(y))))

## using layer styles, r.squared
xyplot(ysim ~ xsim) +
  layer(panel.ablineq(lm(y ~ x), r.sq = TRUE,
                      at = 0.4, adj=0:1), style = 1) +
  layer(panel.ablineq(lm(y ~ x + 0), r.sq = TRUE,
                      at = 0.6, adj=0:1), style = 2)

## alternative placement of equations
xyplot(ysim ~ xsim) +
  layer(panel.ablineq(lm(y ~ x), r.sq = TRUE, rot = TRUE,
                      at = 0.8, pos = 3), style = 1) +
  layer(panel.ablineq(lm(y ~ x + 0), r.sq = TRUE, rot = TRUE,
                      at = 0.8, pos = 1), style = 2)

\dontshow{ .featured_example <- TRUE }
update(trellis.last.object(),
  auto.key = list(text = c("intercept", "no intercept"),
                  points = FALSE, lines = TRUE))
}
\keyword{ aplot }
