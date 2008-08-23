\name{c.trellis}
\alias{c.trellis}
\title{ Merge trellis objects }
\description{
  Combine the panels of multiple trellis objects into one.
}
\usage{
\S3method{c}{trellis}(..., x.same = FALSE, y.same = FALSE, recursive = FALSE)
}
\arguments{
  \item{\dots}{ two or more trellis objects. If these are
    named arguments, the names will be used in the corresponding
    panel strips.
  }
  \item{x.same}{ if \code{TRUE}, fix the x scales to those in the
    first object, as long as that has
    \code{scales$relation = "same"}.
    Otherwise, the x scales in each panel
    will be as they were in the original objects
    (so in general not the same).
  }
  \item{y.same}{ as above, for y scales. }
  \item{recursive}{ for consistency with the generic method, ignored. }
}
\details{
  This mechanism attempts to merge the panels from multiple trellis
  objects into one. The same effect could generally be achieved by
  either a custom panel function (where the display depends on
  \code{packet.number()}), or using \code{\link[lattice]{print.trellis}}
  to display multiple trellis objects. However, in some cases it is more
  convenient to use \code{c()}. Furthermore, it can be useful to
  maintain the display as a standard lattice display, rather than a
  composite using \code{\link[lattice]{print.trellis}}, to simplify
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
\seealso{ \code{\link{marginals}} was the original motivating application,
  \code{\link[lattice]{print.trellis}},
  \code{\link[lattice]{update.trellis}},
  \code{\link[lattice]{trellis.object}} }
\examples{

## combine different types of plots
c(wireframe(volcano), contourplot(volcano))

## a table as both frequencies and proportions
## (together contains the same information as mosaic plot)
## remove last row (containing totals)

data(postdoc)
postdoc <- postdoc[1:(nrow(postdoc)-1),]
pdprops <- barchart(prop.table(postdoc, margin=1), xlab="Proportion",
                   auto.key=list(adj=1))
pdmargin <- barchart(margin.table(postdoc, 1))
pdboth <- c(pdprops, pdmargin)
update(pdboth, xlab=c("Proportion", "Freq"))

## conditioned 'quakes' plot combined with histogram
qua <- xyplot(lat ~ long | equal.count(depth, 3), quakes,
    aspect="iso", pch=".", cex=2, xlab=NULL, ylab=NULL)
qua <- c(qua, depth=histogram(quakes$depth))
## suppress scales on the first 3 panels
update(qua, scales=list(at=c(rep(list(NULL), 3), NA),
                 y=list(draw=FALSE)))

## visualise statistical and spatial distributions
vars <- as.data.frame(state.x77)
StateName <- tolower(state.name)
form <- StateName ~ Population + Income + Illiteracy +
   `Life Exp` + Murder + `HS Grad` + Frost + sqrt(Area)
## construct independent maps of each variable
library(maps)
statemap <- map("state", plot = FALSE, fill = TRUE)
statemap$names <- gsub(":.*", "", statemap$names)
colkey <- draw.colorkey(list(col=heat.colors(100),
    at=0:100, labels=list(labels=c("min","max"), at=c(0,100))))
panel.mapplot.each <- function(x, breaks, ...)
   panel.mapplot(x=x, breaks=quantile(x), ...)
vmaps <- mapplot(form, vars, map=statemap, colramp=heat.colors,
  panel=panel.mapplot.each, colorkey=FALSE,
  legend=list(right=list(fun=colkey)), xlab=NULL)
## construct independent densityplots of each variable
vdens <- densityplot(form[-2], vars, outer=TRUE,
   prepanel=function(...)
      list(xlim=c(0, max(prepanel.default.densityplot(...)$xlim))),
   scales=list(relation="free", x=list(axs="i")), cex=0.5, ref=TRUE)
## combine panels from both plots
combo <- c(vmaps, vdens)
## rearrange in pairs
n <- length(vars)
npairs <- rep(1:n, each=2) + c(0,n)
update(combo[npairs], scales=list(draw=FALSE),
       layout=c(4,4), between = list(x = c(0, 0.5), y = 0.5))

## this example can be done with lattice anyway, as something like:
## xyplot(Petal.Length + Sepal.Length ~ Petal.Width + Sepal.Width,
##        iris, groups=Species, outer=TRUE, scales="free")[c(1,4)]
## but here is another way to do it:
sepals <- xyplot(Sepal.Length ~ Sepal.Width, iris, groups=Species,
  xlab="Width", ylab="Height")
petals <- xyplot(Petal.Length ~ Petal.Width, iris, groups=Species)
c(Sepals=sepals, Petals=petals)
## if 'same' scales are requested, these are taken from first object
c(Sepals=sepals, Petals=petals, x.same=TRUE, y.same=TRUE)

}
\keyword{ aplot }
