\name{custom.theme}
\alias{custom.theme}
\title{ Create a lattice theme based on specified colors }
\description{
  Creates a lattice theme given a few colors.  Non-color settings 
  are not included.  The colors are typically used to define the
  standard grouping (superposition) colors, and the first color is used
  for ungrouped displays.
}
\usage{
custom.theme(symbol = brewer.pal(n = 8, name = "Dark2"),
             fill = brewer.pal(n = 12, name = "Set3"),
             region = brewer.pal(n = 11, name = "Spectral"),
             reference = "#e8e8e8",
             bg = "transparent", fg = "black")
}
\arguments{
  \item{symbol}{ a vector of symbol colors.   }
  \item{fill}{ a vector of fill colors (for barcharts, etc.) }
  \item{region}{ a vector of colors that is used to define a continuous
    color gradient using \code{\link{colorRampPalette}}}
  \item{reference}{ a color for reference lines and such  }
  \item{bg}{ a background color }
  \item{fg}{ a foreground color, primarily for annotation }
}
\value{

  A list that can be supplied to
  \code{\link[lattice:trellis.par.get]{trellis.par.get}} or as the
  \code{theme} argument to
  \code{\link[lattice:trellis.device]{trellis.device}}. 

}
\author{ Deepayan Sarkar }
\keyword{dplot}
