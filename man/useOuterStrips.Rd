\name{useOuterStrips}
\alias{useOuterStrips}
\alias{resizePanels}
\title{ Put Strips on the Boundary of a Lattice Display }
\description{

  Tried to update a \code{"trellis"} object so that strips are only
  shown on the top and left boundaries when printed, instead of in every
  panel as is usual.  This is only meaningful when there are exactly tow
  conditioning variables.

}
\usage{

useOuterStrips(x,
               strip = strip.default,
               strip.left = strip.custom(horizontal = FALSE),
               strip.lines = 1,
               strip.left.lines = strip.lines)

resizePanels(x, h = 1, w = 1)

}

\arguments{
  \item{x}{ An object of class \code{"trellis"}. }
  \item{strip, strip.left}{ A function, character string or logical that
    would be appropriate \code{strip} and \code{strip.left} arguments
    respectively in a high level lattice function call (see
    \code{\link{xyplot}}) }
  \item{strip.lines, strip.left.lines}{ height of strips in number of
    lines; helpful for multi-line text or mathematical annotation in
    strips. }
  \item{h}{ numeric vector specifying panel heights}
  \item{w}{ numeric vector specifying of panel widths}
}

\details{
  \code{useOuterStrips} modifies a \code{"trellis"} object with
  \code{length(dim(x)) == 2} so that when plotted, strips are only shown
  on the top and left boundaries of the panel layout, rather than on top
  of every panel, as is the usual behaviour.

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

\seealso{ \code{\link[lattice:Lattice]{Lattice}},
  \code{\link[lattice:xyplot]{xyplot}} }

\examples{

library(lattice)

mtcars$HP <- equal.count(mtcars$hp)

useOuterStrips(xyplot(mpg ~ disp | HP + factor(cyl), mtcars))

useOuterStrips(xyplot(mpg ~ disp | factor(cyl) + HP, mtcars),
               strip.left = FALSE,
               strip = strip.custom(style = 4))


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
