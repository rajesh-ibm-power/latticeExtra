\name{doubleYScale}
\alias{doubleYScale}
\title{ Draw two plot series with different y scales }
\description{
  Overplot two trellis objects with different y scales,
  optionally in different styles, adding a second y axis, and/or a
  second y axis label.

  \emph{Note:} drawing plots with multiple scales is often a bad idea
  as it can be misleading.
}
\usage{
doubleYScale(obj1, obj2, use.style = TRUE,
             style1 = if (use.style) 1, style2 = if (use.style) 2,
             add.axis = TRUE, add.ylab2 = FALSE,
             text = NULL, auto.key = if (!is.null(text))
               list(text, points = points, lines = lines, ...),
             points = FALSE, lines = TRUE, ..., under = FALSE)
}
\arguments{
  \item{obj1, obj2}{
    trellis objects. Note that most settings, like main/sub/legend/etc
    are taken only from \code{obj1}; only the panel, axis and ylab are
    taken from obj2.
  }
  \item{use.style, style1, style2}{
    \code{style1} and \code{style2} give the `group number' for obj1
    and obj2 respectively. The style is taken from these indices into
    the values of \code{trellis.par.get("superpose.line")}.
    Therefore these should be integers between 1 and 6;
    a value of 0 or NULL can be given to leave the default settings.
    These will also be applied to the y-axes and ylab, if relevant.
    \code{use.style} simply changes the defaults of the style arguments.
  }
  \item{add.axis}{
    if TRUE, draw a second y axis (for the \code{obj2} series) on the
    right side of the plot.
  }
  \item{add.ylab2}{
    if TRUE, draw a second y axis label (from \code{obj2$ylab}) on the
    right side of the plot. Note, this will replace any existing key or
    legend on the right side, i.e. with \code{space = "right"}.
  }
  \item{text, auto.key, points, lines, \dots}{
    if non-NULL, add a \code{key} to the display, using entries named by
    \code{text}. Further arguments are passed on to
    \code{\link{simpleKey}} at plot time.
  }
  \item{under}{
    if TRUE, draw \code{obj2} under \code{obj1}.
  }
}
\details{
  Panels from the trellis object \code{obj2} will be drawn in the
  corresponding panel of \code{obj1}.

  Axis settings are taken from the trellis objects, so most
  \code{scales} arguments such as \code{draw}, \code{at}, \code{labels}
  etc from \code{obj2} will carry over to the second y axis.
}
\value{
  a merged trellis object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{
  \code{\link{as.layer}}
}
\examples{
set.seed(1)
foo <- list(x = 1:100, y = cumsum(rnorm(100)))
## show original data
xyplot(y + y^2 ~ x, foo, type = "l")
## construct separate plots for each series
obj1 <- xyplot(y ~ x, foo, type = "l")
obj2 <- xyplot(y^2 ~ x, foo, type = "l")
## simple case: no axis for the overlaid plot
doubleYScale(obj1, obj2, add.axis = FALSE)
## draw second y axis
doubleYScale(obj1, obj2)
## ...with second ylab
doubleYScale(obj1, obj2, add.ylab2 = TRUE)
## ...or with a key
doubleYScale(obj1, obj2, text = c("obj1", "obj2"))
## ...with custom styles
update(doubleYScale(obj1, obj2, text = c("obj1", "obj2")),
  par.settings = simpleTheme(col = c('red','black'), lty = 1:2))

## different plot types
x <- rnorm(60)
doubleYScale(histogram(x), densityplot(x), use.style = FALSE)
## (but see ?as.layer for a better way to do this)

## multi-panel example
## a variant of Figure 5.13 from Sarkar (2008)
## http://lmdvr.r-forge.r-project.org/figures/figures.html?chapter=05;figure=05_13
data(SeatacWeather)
temp <- xyplot(min.temp + max.temp ~ day | month,
               data = SeatacWeather, type = "l", layout = c(3, 1))
rain <- xyplot(precip ~ day | month, data = SeatacWeather, type = "h")

doubleYScale(temp, rain, style1 = 0, style2 = 3, add.ylab2 = TRUE,
   text = c("min. T", "max. T", "rain"), columns = 3)

## re-plot with different styles
update(trellis.last.object(),
   par.settings = simpleTheme(col = c("black", "red", "blue")))
}
\keyword{ aplot }
