\name{dendrogramGrob}
\alias{dendrogramGrob}
\title{ Create a Grob Representing a Dendrogram }
\description{

  This function creates a grob (a grid graphics object) that can be
  manipulated as such.  In particular, it can be used as a legend in a
  lattice display like \code{levelplot} to form heatmaps.

}


\usage{
dendrogramGrob(x, ord = order.dendrogram(x),
               side = c("right", "top"),
               add = list(), size = 5, size.add = 1,
               type = c("rectangle", "triangle"),
               \dots)
}

\arguments{
  \item{x}{ An object of class \code{"dendrogram"}.  See
    \code{\link{dendrogram}} for details }

  \item{ord}{ A vector of integer indices giving the order in which the
    terminal leaves are to be plotted.  If this is not the same as
    \code{order.dendrogram(x)}, then the leaves may not cluster together
    and branches of the dendrogram may intersect.  }

  \item{side}{ Intended position of the dendrogram when added in a
    heatmap.  Currently allowed positions are \code{"right"} and
    \code{"top"}. }

  \item{add}{ Additional annotation.  Currently, it is only possible to
    add one or more rows of rectangles at the base of the dendrogram.
    See details below.  }
  \item{size}{ Total height of the dendrogram in \code{"lines"} (see
    \code{\link{unit}})
  }
  \item{size.add}{ Size of each additional row, also in \code{"lines"} }
  \item{type}{ Whether a child node is joined to its parent  directly
    with a straight line (\code{"triangle"}) or as a \dQuote{stair} with
    two lines (\code{"rectangle"})
  }
  \item{\dots}{ Extra arguments.  Currently ignored. }
}
\details{
  The \code{add} argument can be used for additional annotation at the
  base of the dendrogram.  It should be a list with one component for
  each row, with names specifying the type of annotation and components
  specifying the contents.  Currently, the only supported name is
  \code{"rect"} (which can be repeated), producing rectangles.  The
  components in such a case is a list of graphical parameters, possibly
  vectorized, that are passed on to \code{\link{gpar}}.
}
\value{
  An object of class \code{"grob"}
}
\author{ Deepayan Sarkar \email{deepayan.sarkar@r-project.org} }

\seealso{
  \code{\link{heatmap}}, \code{\link{levelplot}}
}

\examples{

data(mtcars)
x  <- t(as.matrix(scale(mtcars)))
dd.row <- as.dendrogram(hclust(dist(x)))
row.ord <- order.dendrogram(dd.row)

dd.col <- as.dendrogram(hclust(dist(t(x))))
col.ord <- order.dendrogram(dd.col)

library(lattice)

levelplot(x[row.ord, col.ord],
          aspect = "fill",
          scales = list(x = list(rot = 90)),
          colorkey = list(space = "left"),
          legend =
          list(right =
               list(fun = dendrogramGrob,
                    args =
                    list(x = dd.col, ord = col.ord,
                         side = "right",
                         size = 10)),
               top =
               list(fun = dendrogramGrob,
                    args =
                    list(x = dd.row, 
                         side = "top",
                         type = "triangle"))))


## Version with unsorted rows.

## For illustration, MPG ploted again at foot of dendrogram

mpg <- x["mpg", col.ord]
mpg.col <- level.colors(mpg, at = do.breaks(range(mpg), 10),
                        col.regions = terrain.colors)

levelplot(x[, col.ord],
          aspect = "iso",
          scales = list(x = list(rot = 90)),
          colorkey = FALSE,
          legend =
          list(right =
               list(fun = dendrogramGrob,
                    args =
                    list(x = dd.col, ord = col.ord,
                         side = "right",
                         add = list(rect = list(fill = mpg.col)),
                         size = 10)),
               top =
               list(fun = dendrogramGrob,
                    args =
                    list(x = dd.row, ord = sort(row.ord),
                         side = "top", size = 10,
                         type = "triangle"))))

}
\keyword{ dplot }

