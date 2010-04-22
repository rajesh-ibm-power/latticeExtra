\name{resizePanels}
\alias{resizePanels}
\title{ Resize panels to match data scales }
\description{

  Modify a \code{"trellis"} object so that when
  plotted, the panels have the specified relative width and height.

}
\usage{

resizePanels(x, h = 1, w = 1)

}

\arguments{
  \item{x}{ An object of class \code{"trellis"}. }
  \item{h}{ numeric vector specifying panel heights}
  \item{w}{ numeric vector specifying of panel widths}
}

\details{

  \code{resizePanels} modifies a \code{"trellis"} object so that when
  plotted, the panels have the specified relative width and height; this
  is only interesting when \code{h} or \code{w} are vectors with unequal
  entries.  \code{resizePanels} can be called with no arguments, in
  which case the currently plotted \code{"trellis"} object (if any) is
  used for \code{x}, and a suitable \code{h} or \code{w} (based on the
  current panel layout) is chosen so that sizes are relative to the
  current panel ranges in the native coordinate system.  This is only
  interesting when \code{scales="free"}; the resulting object, when
  plotted again, will have varying panel sizes but the same number of
  data units per inch in all panels.

}

\value{

  An object of class \code{"trellis"}; essentially the same as \code{x},
  but with certain properties modified.

}
\author{ Deepayan Sarkar }

\seealso{ \code{\link{Lattice}},
  \code{\link{xyplot}} }

\examples{

state <- data.frame(state.x77, state.region, state.name)
state$state.name <-
    with(state, reorder(reorder(state.name, Frost),
                        as.numeric(state.region)))
dpfrost <-
    dotplot(state.name ~ Frost | reorder(state.region, Frost),
            data = state, layout = c(1, 4),
            scales = list(y = list(relation = "free")))

## approximate
resizePanels(dpfrost,
             h = with(state, table(reorder(state.region, Frost))))

## exact (including boundary padding)
resizePanels()

}

\keyword{dplot}
