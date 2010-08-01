\name{combineLimits}
\alias{combineLimits}
\title{Combine axis limits across margins}
\description{

  Modifies a \code{"trellis"} object with \code{"free"} scales so that
  panel limits are extended to be the same across selected conditioning
  variables (typically rows and columns).

}
\usage{
combineLimits(x, margin.x = 2L, margin.y = 1L,
              extend = TRUE, adjust.labels = TRUE)
}

\arguments{
  \item{x}{ An object of class \code{"trellis"}. }
  \item{margin.x}{
    Integer vector specifying which conditioning variables to combine
    the x-axis limits over.  Defaults to the second conditioning
    variable (rows in the default layout); that is, the limit of each
    packet is extended to include the limits of all other packets
    obtained by varying the level of the second conditioning variable
    (row).  More than one variable can be specified; for example,
    \code{margin.x = c(1, 2)} would ensure that limits are extended to
    include all levels in both the first and second conditioning
    variables.  In case there is a third conditioning variable, this
    would have the effect of per-page x-axis limits with the default
    layout.
  }
  \item{margin.y}{
    Integer vector specifying which conditioning variables to combine
    the x-axis limits over.  Similar to \code{margin.x}, but defaults to
    the first conditioning variable (columns in the default layout).
  }
  \item{extend}{
    Logical flag indicating whether the limits should be extended after
    being combined.  Usually a good idea.
  }
  \item{adjust.labels}{
    Logical flag indicating whether labels should be removed from all
    but the boundaries.  This may give misleading plots with non-default
    layouts.
  }
}

\details{
  \code{combineLimits} modifies a \code{"trellis"} object to ... FIXME

  Useful mainly for plots with two conditioning variables with the
  default layout (columns and rows correspond to the first and second
  conditioning variables), when per-row and per-column limits are
  desired.

}

\note{

  Does not work for all \code{"trellis"} objects.  In particular,
  log-scales do not yet work.  Fancy layouts with \code{skip}-ped panels
  and unusual packet-to-panel mappings will probably also not work.

}

\value{

  An object of class \code{"trellis"}; essentially the same as \code{x},
  but with certain properties modified.

}
\author{ Deepayan Sarkar }

\seealso{ \code{\link{Lattice}}, \code{\link{xyplot}} }

\examples{

data(Cars93, package = "MASS")

## FIXME: log scales don't yet work

pcars <- 
    xyplot(Price ~ EngineSize | reorder(AirBags, Price) + Cylinders,
           data = Cars93, 
           subset = Cylinders != "rotary" & Cylinders != "5", 
           scales = list(relation = "free",
                         y = list(log = FALSE, tick.number = 3, rot = 0)),
           xlab = "Engine Size (litres)", 
           ylab = "Average Price (1000 USD)",
           as.table = TRUE) 

combineLimits(pcars)

useOuterStrips(combineLimits(update(pcars, grid = TRUE),
                             margin.x = c(1, 2), adjust.labels = FALSE))

useOuterStrips(combineLimits(update(pcars, grid = TRUE)))

}

\keyword{dplot}
