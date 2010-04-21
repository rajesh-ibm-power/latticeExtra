\name{useOuterStrips}
\alias{useOuterStrips}
\title{ Put Strips on the Boundary of a Lattice Display }
\description{

  Try to update a \code{"trellis"} object so that strips are only
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
}

\details{
  \code{useOuterStrips} modifies a \code{"trellis"} object with
  \code{length(dim(x)) == 2} so that when plotted, strips are only shown
  on the top and left boundaries of the panel layout, rather than on top
  of every panel, as is the usual behaviour.

}

\value{

  An object of class \code{"trellis"}; essentially the same as \code{x},
  but with certain properties modified.

}
\author{ Deepayan Sarkar }

\seealso{ \code{\link{Lattice}},
  \code{\link{xyplot}} }

\examples{

library(lattice)

mtcars$HP <- equal.count(mtcars$hp)

\dontshow{ .featured_example <- "useOuterStrips" }
useOuterStrips(xyplot(mpg ~ disp | HP + factor(cyl), mtcars))

useOuterStrips(xyplot(mpg ~ disp | factor(cyl) + HP, mtcars),
               strip.left = FALSE,
               strip = strip.custom(style = 4))

}

\keyword{dplot}
