\name{theEconomist.theme}
\alias{theEconomist.theme}
\alias{asTheEconomist}
\alias{panel.xticksgrid}
\title{
  Generate plots with style like The Economist.
}
\description{
  Uses colors, drawing styles, axis settings, etc, to produce plots
  inspired by those in The Economist
  magazine. (\url{http://www.economist.com/}).
}
\usage{
asTheEconomist(x, type = "l",
    vertical = FALSE, zeroline = "red",
    par.settings =
      theEconomist.theme(with.bg = with.bg, box = "transparent"),
    with.bg = FALSE,
    titleSpec = list(x = grid::unit(5, "mm"), just = "left"),
    ylab = expression(NULL), xlab = expression(NULL),
    scales = list(axs = "i", x = list(tck = 0, alternating = 1),
                             y = list(tck = 0, alternating = 2)),
    par.strip.text = list(font = 2),
    between = list(x = 1, y = 1))

theEconomist.theme(win.fontfamily = NULL,
    with.bg = FALSE, box = "black", \dots)
}
\arguments{
  \item{x}{
    a \code{trellis} object, i.e. the result of a high-level plot
    function in the Lattice framework.
  }
  \item{type}{
    plot type, relevant for \code{xyplot}s, see \code{\link{panel.xyplot}}.
  }
  \item{vertical}{
    to draw vertical grid lines rather than the default horizontal.
  }
  \item{zeroline}{
    color for a grid line to draw at zero on the x axis (or y axis if
    \code{vertical = TRUE}. Set to \code{NA} to omit.
  }
  \item{par.settings}{
    style settings, defaulting to \code{theEconomist.theme}.
  }
  \item{with.bg}{
    if \code{TRUE}, uses a light blue background and a few other
    corresponding changes; otherwise white.
  }
  \item{titleSpec}{
    position and justification settings to be used in the \code{main}
    and \code{sub} arguments to \code{xyplot}.
  }
  \item{ylab, xlab}{
    axis labels, blank by default.
  }
  \item{scales, par.strip.text, between}{
    see \code{\link{xyplot}}.
  }
  \item{win.fontfamily}{
    on Windows systems, sets the font by name.
  }
  \item{box}{
    color for panel boxes, strip outlines, and axis ticks.
  }
  \item{\dots}{
    further arguments passed to \code{\link{simpleTheme}}
    and used to modify the theme.
  }
}
\details{
  You can just use \code{par.settings = theEconomist.theme()}, which
  gives you the colors and styles, but it does not do the
  grid lines, axis settings, or title settings.
}
\references{
  \url{http://www.economist.com/displayStory.cfm?story_id=15065782}

  \url{http://www.economist.com/displayStory.cfm?story_id=14941181}
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
  \code{\link{custom.theme}}
}
\examples{
asTheEconomist(xyplot(window(sunspot.year, start = 1900),
  main = "Sunspot cycles", sub = "Number per year",
  scales = list(y = list(relation = "same", axs = "i"))))

\dontshow{ .featured_example <- TRUE }
trellis.last.object() +
  layer_(panel.xblocks(x, x >= 1980, col = "#6CCFF6", alpha = .5)) +
  layer(panel.text(1988, 180, "Forecast", font = 3, pos = 2))

asTheEconomist(
  barchart(Titanic[,,,"No"], main = "Titanic deaths", layout = 1:2,
    sub = "by sex and class", auto.key = list(columns = 2)),
  vertical = TRUE)

asTheEconomist(
  dotplot(VADeaths, main = "Death Rates in Virginia (1940)",
    auto.key = list(x = .9, y = .1, corner = c(1,0))),
  type = "b", with.bg = TRUE)

dotplot(VADeaths, auto.key = TRUE, type = "b",
  par.settings = theEconomist.theme(with.bg = TRUE))

asTheEconomist(
  densityplot(~ height, groups = voice.part, data = singer,
    subset = grep("1", voice.part), plot.points = FALSE)) +
glayer(d <- density(x), i <- which.max(d$y),
  ltext(d$x[i], d$y[i], paste("Group", group.number), pos = 3))
}
\keyword{ dplot }
