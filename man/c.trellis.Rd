\name{c.trellis}
\alias{c.trellis}
\alias{xyplot.list}
\title{ Merge trellis objects, using same or different scales }
\description{
  Combine the panels of multiple trellis objects into one.
}
\usage{
\method{c}{trellis}(..., x.same = NA, y.same = NA,
    layout = NULL, recursive = FALSE)

xyplot.list(x, data = NULL, ..., FUN = xyplot,
    y.same = TRUE, x.same = NA, layout = NULL)
}
\arguments{
  \item{\dots}{ two or more trellis objects. If these are
    named arguments, the names will be used in the corresponding
    panel strips.
  }
  \item{x.same}{ if \code{TRUE}, set the x scale relation to
    \code{"same"} and recalculate panel limits using data
    from all panels. Otherwise, the x scales in each panel
    will be as they were in the original objects
    (so in general not the same), the default behaviour.
  }
  \item{y.same}{ as above, for y scales.
    Note that \code{xyplot.list} defaults to same y scales. Set to
    \code{NA} to leave them alone. }
  \item{layout}{ value for \code{layout} of the new plot; see
    \code{\link{xyplot}}. }
  \item{recursive}{ for consistency with the generic method, ignored. }
  \item{x}{ a \code{list}; the function \code{FUN}, which defaults to
    \code{\link{xyplot}}, will be called on each element of \code{x}, and the
    resulting plots combined into one. }
  \item{FUN, data}{ a lattice plot function, to be called on each element of
    the list \code{x}, along with \code{data} and \code{...} }
}
\details{
  This mechanism attempts to merge the panels from multiple trellis
  objects into one. The same effect could generally be achieved by
  either a custom panel function (where the display depends on
  \code{packet.number()}), or using \code{\link{print.trellis}}
  to display multiple trellis objects. However, in some cases it is more
  convenient to use \code{c()}. Furthermore, it can be useful to
  maintain the display as a standard lattice display, rather than a
  composite using \code{\link{print.trellis}}, to simplify
  further interaction.

  Many properties of the display, such as titles, legends, axis settings
  and aspect ratio will be taken from the first object only.

  Note that combining panels from different types of plots does not
  really fit the trellis model. Some features of the plot may not work
  as expected. In particular, some work may be needed to
  show or hide scales on selected panels. An example is given below.

  Any trellis object with more than one conditioning variable will be
  "flattened" to one dimension, eliminating the multi-variate
  conditioning structure.
}
\value{
  a new trellis object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{
  \code{\link{marginal.plot}} was the original motivating application,
  \code{\link{print.trellis}},
  \code{\link{update.trellis}},
  \code{\link{trellis.object}}
}
\examples{

## Combine different types of plots.
c(wireframe(volcano), contourplot(volcano))

## Merging levelplot with xyplot
levObj <- levelplot(prop.table(WorldPhones, 1) * 100)
xyObj <- xyplot(Phones ~ Year, data.frame(Phones = rowSums(WorldPhones),
      Year = row.names(WorldPhones)), type="b", ylim = c(0, 150000))
## NOTE: prepanel.levelplot (from first object) is used for entire plot.
cObj <- c(levObj, xyObj, layout = 1:2)
update(cObj, scales = list(y = list(rot = 0)),
       ylab = c("proportional distribution", "number of phones"))

## Combine two xyplots.
sepals <- xyplot(Sepal.Length ~ Sepal.Width, iris, groups = Species,
    xlab = "Width", ylab = "Height")
petals <- xyplot(Petal.Length ~ Petal.Width, iris, groups = Species)
c(Sepals = sepals, Petals = petals)

## Force same scales (re-calculate panel limits from merged data):
c(Sepals = sepals, Petals = petals, x.same = TRUE, y.same = TRUE)

## Or - create xyplots from a list of formulas
xyplot.list(list(Sepals = Sepal.Length ~ Sepal.Width,
                 Petals = Petal.Length ~ Petal.Width),
             data = iris, groups = Species, x.same = TRUE,
             xlab = "Width", ylab = "Height")

## Create histograms from a list of objects, and merge them.
xyplot.list(iris, FUN = histogram)

## Create cumulative distribution plots from a list of objects
xyplot.list(iris[1:4], FUN = qqmath, groups = iris$Species,
            auto.key = TRUE)

## Display a table as both frequencies and proportions:
data(postdoc)
## remove last row (containing totals)
postdoc <- postdoc[1:(nrow(postdoc)-1),]
pdprops <- barchart(prop.table(postdoc, margin = 1),
                    auto.key = list(adj = 1))
pdmargin <- barchart(margin.table(postdoc, 1))
pdboth <- c(pdprops, pdmargin)
update(pdboth, xlab = c("Proportion", "Freq"))

\dontshow{ .featured_example <- TRUE }
## Conditioned 'quakes' plot combined with histogram.
qua <- xyplot(lat ~ long | equal.count(depth, 3), quakes,
    aspect = "iso", pch = ".", cex = 2, xlab = NULL, ylab = NULL)
qua <- c(qua, depth = histogram(quakes$depth), layout = c(4, 1))
## suppress scales on the first 3 panels
update(qua, scales = list(at = list(NULL, NULL, NULL, NA),
                 y = list(draw = FALSE)))

## Visualise statistical and spatial distributions
## (advanced!)
library(maps)
vars <- as.data.frame(state.x77)
StateName <- tolower(state.name)
form <- StateName ~ Population + Income + Illiteracy +
   `Life Exp` + Murder + `HS Grad` + Frost + sqrt(Area)
## construct independent maps of each variable
statemap <- map("state", plot = FALSE, fill = TRUE)
statemap$names <- gsub(":.*", "", statemap$names)
colkey <- draw.colorkey(list(col = heat.colors(100),
    at = 0:100, labels = list(labels = c("min","max"), at = c(0,100))))
panel.mapplot.each <- function(x, breaks, ...)
   panel.mapplot(x = x, breaks = quantile(x), ...)
vmaps <- mapplot(form, vars, map = statemap, colramp = heat.colors,
  panel = panel.mapplot.each, colorkey = FALSE,
  legend = list(right = list(fun = colkey)), xlab = NULL)
## construct independent densityplots of each variable
vdens <- densityplot(form[-2], vars, outer = TRUE,
   prepanel = function(...)
      list(xlim = c(0, max(prepanel.default.densityplot(...)$xlim))),
   scales = list(relation = "free", x = list(axs = "i")),
   cex = 0.5, ref = TRUE)
## combine panels from both plots
combo <- c(vmaps, vdens)
## rearrange in pairs
n <- length(vars)
npairs <- rep(1:n, each = 2) + c(0, n)
update(combo[npairs], scales = list(draw = FALSE),
       layout = c(4, 4), between = list(x = c(0, 0.5), y = 0.5))
}
\keyword{ aplot }
