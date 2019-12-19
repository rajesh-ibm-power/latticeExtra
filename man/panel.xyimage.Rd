\name{panel.xyimage}
\alias{panel.xyimage}
\title{
  Scatter plots using emoji-like images
}
\description{
  Panel function that create scatter plots with emoji-like images for
  plotting character. Images can be local files or URLs. Only PNG and
  JPEG images are currently supported.
}
\usage{
panel.xyimage(x, y, 
              subscripts,
              groups = NULL,
              pch = NULL,
              cex = 1,
              ...,
              grid = FALSE, abline = NULL)
}
\arguments{
  \item{x, y}{
    Data vectors to be plotted.
  }
  \item{subscripts}{
    An integer vector of subscripts giving indices of the \code{x} and
    \code{y} values in the original data source.  See the corresponding
    entry in \code{\link{xyplot}} for details.
  }
  \item{groups}{
    A factor defining groups.
  }
  \item{pch}{
    A character vector giving path(s) or URL(s) of PNG or JPEG files. If 
    \code{groups} is specified, there should be one value for each
    level, otherwise this should have length 1. Recycled as necessary.
  }
  \item{cex}{
    A numeric multiplier for the size of the symbols. As with
    \code{pch}, this can be a vector corresponding to levels of
    \code{groups}.
  }
  \item{\dots}{
    Further arguments are accepted but ignored.
  }
  \item{grid}{
    A logical flag, character string, or list specifying whether and how
    a background grid should be drawn. See \code{\link{panel.xyplot}}
    for details.
  }
  \item{abline}{
    A numeric vector or list, specifying arguments arguments for
    \code{\link{panel.abline}}, which is called with those
    arguments. See \code{\link{panel.xyplot}}
    for details.
  }
}
\details{
  The image sources given by \code{pch} are downloaded if necessary and
  read in every time the panel function is called. If the same images
  are to be used in multiple panels, it may be more efficient to
  download them once and provide the file paths rather than provide
  URLs.
}
\author{
  Deepayan Sarkar
}
\seealso{
  \code{\link{panel.xyplot}}
}
\examples{
\dontrun{
alive <- "https://twemoji.maxcdn.com/72x72/1f60a.png"
dead <- "https://twemoji.maxcdn.com/72x72/1f480.png"

dotplot(Titanic,
        scales = list(x = "free"),
        between = list(x = 1),
        panel = panel.xyimage,
        pch = c(dead, alive), grid = "h", 
        main = "Survival on the Titanic",
        xlab = "Number of persons")
}
}
\keyword{ dplot }
