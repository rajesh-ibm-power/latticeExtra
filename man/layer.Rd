\name{layer}
\alias{layer}
\alias{layer_}
\alias{glayer}
\alias{glayer_}
\alias{+.trellis}
\alias{drawLayer}
\alias{flattenPanel}
\alias{[.layer}
\alias{print.layer}
\title{ Add layers to a Trellis plot, optionally using a new data source }
\description{
  A mechanism to add new layers to a trellis object, optionally using a
  new data source. This is an alternative to modifying the
  panel function. Note the non-standard evaluation in \code{layer()}.
}
\usage{
layer(..., data, magicdots, exclude,
      packets, rows, columns, groups,
      style, force, theme, under, superpose, 
      eval, etc) ## these 2 deprecated

layer_(...)
glayer(...)
glayer_(...)

\S3method{+}{trellis}(object, lay)

drawLayer(lay)

flattenPanel(object)
}
\arguments{
  \item{\dots}{
    expressions as they would appear in a panel function. These can
    refer to the panel function arguments (such as \code{x}, \code{y}
    and \code{subscripts}), and also to any named objects passed in
    through the \code{data} argument. The calls can also include the
    special argument \dQuote{\code{\dots}}; in the default case of
    \code{magicdots = TRUE}, only those arguments which are not already
    named in a call are passed on through \dQuote{\code{\dots}}.
    Otherwise, \dQuote{\code{\dots}} simply represents all panel
    function arguments. See Details, below.
  }
  \item{data}{
    optional. A named \code{list} containing objects needed when
    evaluating (drawing) the layer.
  }
  \item{magicdots, exclude}{
    if \code{magicdots = TRUE}, the default, any reference to
    \dQuote{\code{\dots}} 
    in the layer expressions will only pass on those arguments from the
    panel function which are not named in the call (thus avoiding 
    duplicate argument errors). If the first argument in a call is not
    named, it is assumed to be named \code{"x"}, and if the second
    argument is not named it is assumed to be named
    \code{"y"}. Furthermore, any argument names given in \code{exclude}
    will not be passed on through \dQuote{\code{\dots}}.
  }
  \item{packets, rows, columns, groups}{
    restricts the layer to draw only in specified packets (which refer
    to individual panels, but are independent of their layout), or rows
    or columns of the trellis layout (\code{\link{trellis.currentLayout}}).
    For group layers (using \code{glayer} or \code{superpose = TRUE}),
    the groups can be restricted also, by specifying group numbers (or
    group values, as character strings). Negative values exclude the
    given items. 
  }
  \item{style}{
    style index of the layer, used only to set lattice graphical
    parameters (same effect as in grouped displays). Note that this will
    use the theme settings in effect in the existing plot, which may or
    may not be what is desired. It may be necessary to use \code{force =
      TRUE} to escape from the plot's settings and use the current
    theme. 
  }
  \item{force}{
    \code{force = TRUE} is just a shorthand for
    \code{theme = trellis.par.get()}, which is useful for over-riding
    the theme settings in effect in an existing plot. For instance, if
    the original plot specified \code{par.settings = simpleTheme(col =
      "red")} then the theme settings in effect will be entirely
    red. Use \code{force = TRUE} to reset the current theme for this
    layer, or use \code{theme} directly.
  }
  \item{theme}{
    a style specification to be passed to \code{\link{trellis.par.set}}
    which has effect only while drawing the layer. One can pass a whole
    theme specification list, such as \code{theme = custom.theme()}, or
    a more specific list, such as \code{theme = simpleTheme(col = "red")}.
  }
  \item{under}{
    whether the layer should be drawn before the existing panel
    function. This defaults to \code{TRUE} in the convenience functions
    \code{layer_()} and \code{glayer_()}.
  }
  \item{superpose}{
    if \code{TRUE}, the layer will be drawn once for
    each level of any \code{groups} in the plot, using
    \code{\link{panel.superpose}}. This defaults to \code{TRUE} in the
    convenience functions \code{glayer()} and \code{glayer_()}.
  }
  \item{eval, etc}{
    These two arguments are now Deprecated and should not be used in new
    code. Rather than using \code{eval}, use \code{data}. It is not
    necessary to use \code{etc} because that happens automatically with
    \code{magicdots}. 
    if \code{eval = TRUE}, arguments given in the layer code will be
    evaluated  immediately (in the calling environment). 
    If \code{etc = TRUE} then all remaining (un-named) panel function
    arguments are added on to the call. If \code{etc} is a character
    vector then only those named arguments are passed through.
  }
  \item{object}{ a trellis object. }
  \item{lay}{ a layer object. }
}
\details{
  The \code{layer} mechanism is a method for augmenting a panel
  function. It allows expressions to be added to the panel function
  without knowing what the original panel function was. In this way it
  can be useful for convenient augmentation of trellis plots.

  Note that the evaluation used in \code{layer} is non-standard, and can
  be confusing at first: you typically refer to variables as if inside
  the panel function (\code{x}, \code{y}, etc); you can usually refer to
  objects which exist in the global environment (workspace), but it is
  safer to pass them in by name in the \code{data} argument to
  \code{layer}. (And this should not to be confused with the \code{data}
  argument to the original \code{xyplot}.)

  A simple example is adding a reference line to each panel:
  \code{layer(panel.refline(h = 0))}. Note that the expressions are
  quoted, so if you have local variables they will need to be either
  accessible globally, or passed in via the \code{data} argument. For
  example:

  \code{layer(panel.refline(h = myVal)) ## if myVal is global}

  \code{layer(panel.refline(h = h), data = list(h = myVal))}

  Another non-standard aspect is that the special argument
  \dQuote{\code{\dots}} will, by default, only pass through those
  argument not already named. For example, this will over-ride the
  \code{x} argument and pass on the remaining arguments:

  \code{layer(panel.xyplot(x = jitter(x), ...))}

  The first un-named argument is assumed to be "x", so that is the same as

  \code{layer(panel.xyplot(jitter(x), ...))}

  The layer mechanism should probably still be considered experimental.

  \code{drawLayer()} actually draws the given layer object, applying the
  panel specification, style settings and so on. It should only be
  called while a panel is in focus.

  The \code{flattenPanel} function will construct a human-readable
  function incorporating code from all layers (and the original panel
  function). Note that this does not return a usable function, as it
  lacks the correct argument list and ignores any extra data sources
  that layers might use. It is intended be edited manually.
}
\value{
  a \code{layer} object is defined as a list of expression objects,
  each of which may have a set of attributes. The result of "adding"
  a layer to a trellis object (\code{+.trellis}) is the updated trellis
  object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{
  \code{\link{update.trellis}},
  \code{\link{as.layer}} for overlaying entire plots
}
\examples{
foo <- xyplot(ozone ~ wind, environmental)
foo

## overlay reference lines
foo <- foo + layer(panel.abline(h = 0)) +
             layer(panel.lmline(x, y, lty = 2))

## underlay a flat color
foo <- foo + layer(panel.fill(grey(.98)), under = TRUE)
foo

## layers can access the panel function arguments
foo <- foo + layer({ ok <- (y>100);
            panel.text(x[ok], y[ok], y[ok], pos = 1) })
foo

## over-ride arguments by name
foo <- foo +
  layer(panel.xyplot(y = ave(y, x, FUN = max), type = "a", ...))
foo

## see a sketch of the complete panel function
flattenPanel(foo)


## group layers, drawn for each group in each panel
dotplot(VADeaths, type = "o") +
  glayer(ltext(x[5], y[5], group.value, srt = 40))

## a quick way to print out the panel.groups arguments:
dotplot(VADeaths, type = "o") + glayer(str(list(...)))


## layers with superposed styles
zoip <- xyplot(ozone ~ wind | equal.count(temperature, 2),
        data = environmental) +
   layer(panel.loess(x, y, span = 0.5), style = 1) +
   layer(panel.loess(x, y, span = 1.0), style = 2)
update(zoip, auto.key = list(text = c("span = 0.5", "span = 1.0"),
   title = "loess smooth", lines = TRUE, points = FALSE))

## note that styles come from the settings in effect in the plot,
## which is not always what you want:
xyplot(1:10 ~ 1:10, type = "b", par.settings = simpleTheme(col = "red")) +
  layer(panel.lines(x = jitter(x, 2), ...)) +  ## drawn in red
  layer(panel.lines(x = jitter(x, 2), ...), force = TRUE)  ## reset theme


## using other variables from the original `data` object
## NOTE: need subscripts = TRUE in original call!
zoip <- xyplot(wind ~ temperature | equal.count(radiation, 2),
   data = environmental, subscripts = TRUE)
zoip + layer(panel.points(..., pch = 19,
             col = grey(1 - ozone[subscripts] / max(ozone))),
       data = environmental)


## restrict drawing to specified panels
barchart(yield ~ variety | site, data = barley,
         groups = year, layout = c(1,6), as.table = TRUE,
         scales = list(x = list(rot = 45))) +
layer(ltext(tapply(y, x, max), lab = abbreviate(levels(x)),
      pos = 3), rows = 1)


## example of a new data source
qua <- xyplot(lat ~ long | cut(depth, 2), quakes,
    aspect = "iso", pch = ".", cex = 2)
qua
\dontshow{ .featured_example <- TRUE }
## add layer showing distance from Auckland
newdat <- with(quakes, expand.grid(
            gridlat = seq(min(lat), max(lat), length = 60),
            gridlon = seq(min(long), max(long), length = 60)))
newdat$dist <- with(newdat, sqrt((gridlat - -36.87)^2 +
                                 (gridlon - 174.75)^2))
qua + layer_(panel.contourplot(x = gridlon, y = gridlat, z = dist,
   contour = TRUE, subscripts = TRUE), data = newdat)
}
\keyword{ aplot }
