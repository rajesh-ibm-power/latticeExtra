\name{ggplot2like.theme}
\alias{ggplot2like.theme}
\alias{ggplot2like.opts}
\alias{ggplot2like}
\alias{axis.grid}
\title{A ggplot2-like theme for Lattice}
\description{
  A theme for Lattice based on some of the default styles used in the
  \pkg{ggplot2} package by Hadley Wickham. Specifically, parts of the
  functions \code{scale_colour_hue}, \code{scale_colour_gradient} and
  \code{theme_gray} were copied. Although superficially similar, the
  implementation here lacks much of the flexibility of the \pkg{ggplot2}
  functions: see \url{http://had.co.nz/ggplot2/}.
}

\usage{
ggplot2like(..., n = 6, h = c(0,360) + 15, l = 65, c = 100,
            h.start = 0, direction = 1,
            low = "#3B4FB8", high = "#B71B1A", space = "rgb")

ggplot2like.opts()

axis.grid(side = c("top", "bottom", "left", "right"),
          ..., ticks = c("default", "yes", "no"),
          scales, components, line.col)
}
\arguments{
  \item{\dots}{
    further arguments passed on to \code{\link{simpleTheme}} to
    over-ride defaults.
  }
  \item{n}{
    number of \code{superpose} styles to generate, with equally spaced hues.
  }
  \item{h, c, l, h.start, direction}{
    range of hues, starting hue and direction to generate a discrete
    colour sequence with \code{\link{hcl}}.
  }
  \item{low, high, space}{
    extreme colors to interpolate with \code{\link{colorRampPalette}}
    for a continuous color scale.
  }
  \item{side, ticks, scales, components, line.col}{
    see \code{\link{axis.default}}. Typically \code{axis.grid} is not
    called directly so these should not be needed.
  }
}

\value{
  \code{ggplot2like()} produces a list of settings which can be
  passed as the \code{par.settings} argument to a high-level Lattice
  plot, or to \code{\link{trellis.par.set}}.
  \code{ggplot2like.opts()} produces a list which can be passed as the
  \code{lattice.options} argument to a high-level Lattice plot, or to
  \code{\link{lattice.options}}. 

}
\author{
  Felix Andrews \email{felix@nfrac.org};
  copied and adapted from \pkg{ggplot2} by Hadley Wickham.
}
\seealso{
  the \pkg{ggplot2} package: \url{http://had.co.nz/ggplot2/}.
  
  \code{\link{custom.theme}}, \code{\link{scale.components}}
}

\examples{
set.seed(0)

## basic theme does not include white grid lines
xyplot(exp(1:10) ~ 1:10, type = "b", 
    par.settings = ggplot2like())

## add lines for axis ticks with custom axis function
xyplot(exp(1:10) ~ 1:10, type = "b", 
    par.settings = ggplot2like(), axis = axis.grid)
    
## this can be used together with scale.components
## (minor lines only visible on devices supporting translucency)
xyplot(exp(rnorm(500)) ~ rnorm(500),
    scales = list(y = list(log = TRUE)),
    yscale.components = yscale.components.log10ticks,
    par.settings = ggplot2like(), axis = axis.grid)

## ggplotlike.opts() specifies axis = axis.grid as well as
## xscale.components.subticks / yscale.components.subticks

xyg <- make.groups(
    "group one" = rnorm(80, 1),
    "group two" = rnorm(80, 5),
    "group three" = rnorm(80, 2))
xyg$x <- rev(xyg$data)

## group styles: specify number of equi-spaced hues
xyplot(data ~ x, xyg, groups = which, auto.key = TRUE, 
    par.settings = ggplot2like(n = 3),
    lattice.options = ggplot2like.opts()) +
  glayer(panel.smoother(...))

## or set it as the default:
opar <- trellis.par.get()
trellis.par.set(ggplot2like(n = 4, h.start = 180))
oopt <- lattice.options(ggplot2like.opts())

bwplot(voice.part ~ height, data = singer)

histogram(rnorm(100))

barchart(Titanic[,,,"No"], main = "Titanic deaths",
    layout = c(1, 2), auto.key = list(columns = 2))

## reset
trellis.par.set(opar)
lattice.options(oopt)

## axis.grid and scale.components.subticks can be used alone:
## (again, lines for minor ticks need transculency-support to show up)
xyplot(exp(1:10) ~ 1:10, type = "b", 
    lattice.options = ggplot2like.opts(),
    par.settings = list(axis.line = list(col = NA),
        reference.line = list(col = "grey")),
    scales = list(tck = c(0,0)))
}

\keyword{dplot}
