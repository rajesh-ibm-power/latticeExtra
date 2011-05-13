\name{panel.scaleArrow}
\alias{panel.scaleArrow}
\title{
  Draw a scale bar as an arrow, labelled with its length in plot units.
}
\description{
  Draw a scale bar as an arrow, labelled with its length in plot units.
}
\usage{
panel.scaleArrow(x = unit(0:1, "npc"), y = unit(0:1, "npc"),
                 default.units = "npc",
                 digits = 0, append = "", label = NULL,
                 angle = 30, length = 0.5, unit = "char",
                 type = "open", ends = "both",
                 \dots,
                 col = add.line$col, fill = col, alpha = add.line$alpha,
                 lty = add.line$lty, lwd = add.line$lwd,
                 col.text = add.text$col, alpha.text = add.text$alpha)
}
\arguments{
  \item{x, y, default.units}{
    coordinates of the line ends as \pkg{grid} \code{\link{unit}}s or
    otherwise interpreted in \code{default.units}.
  }
  \item{digits}{
    number of decimal places to keep for the distance measure.
  }
  \item{append}{
    a string to append to the distance for the label.
  }
  \item{label}{
    label to place on the mid point of the scale, over-riding the default.
  }
  \item{angle, length, unit, type, ends}{
    specification of the arrow style; see \code{\link{panel.arrows}}.
  }
  \item{\dots}{
    further arguments passed to \code{\link{panel.text}}.
    You will need at least the \code{pos} or \code{adj} arguments.
  }
  \item{col, fill, alpha, lty, lwd}{
    graphical parameters relevant to the line.
  }
  \item{col.text, alpha.text}{
    graphical parameters relevant to the text label. Others like
    \code{cex} and \code{font} can be passed though \code{\dots}.
  }
}
\author{
  Felix Andrews \email{felix@nfrac.org}
}
\seealso{
  \code{\link{panel.abline}},
  \code{\link{grid.text}}
}
\examples{
xyplot(EuStockMarkets) +
  layer(panel.scaleArrow(x = 0.99, append = " units",
          col = "grey", srt = 90, cex = 0.8))
}
\keyword{ aplot }
