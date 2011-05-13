\name{marginal.plot}
\alias{marginal.plot}
\title{ Display marginal distributions }
\description{
  Display marginal distributions of several variables,
  which may be numeric and/or categorical, on one plot.
}
\usage{
marginal.plot(x,
              data = NULL,
              groups = NULL,
              reorder = !is.table(x),
              plot.points = FALSE,
              ref = TRUE, cut = 0,
              origin = 0, %ylim = c(0, NA),
              xlab = NULL, ylab = NULL,
              type = c("p", if (is.null(groups)) "h"),
              ...,
              subset = TRUE,
              as.table = TRUE,
              subscripts = TRUE,
              default.scales = list(
                relation = "free",
                abbreviate = TRUE, minlength = 5,
                rot = 30, cex = 0.75, tick.number = 3,
                y = list(draw = FALSE)),
             layout = NULL,
             lattice.options = list(
               layout.heights = list(
                 axis.xlab.padding = list(x = 0),
                 xlab.key.padding = list(x = 0))))
}
\arguments{
  \item{x}{ a data frame or table, or a formula of which the first term
    is a data frame or table. Otherwise coerced with
    \code{as.data.frame}. }
  \item{data}{ an optional data source in which groups and subset may be
    be evaluated. }
  \item{groups}{ term, to be evaluated in \code{data}, that is used as a
    grouping variable. }
  \item{reorder}{ whether to reorder factor variables by frequency. }
  \item{subset}{ data subset expression, evaluated in \code{data}. }
  \item{plot.points, ref, cut}{ passed to \code{panel.densityplot}. }
  \item{origin, type}{ passed to \code{panel.dotplot}. }
  \item{xlab, ylab, as.table, subscripts}{
    see \code{\link{xyplot}}.
  }
  \item{default.scales, layout, lattice.options}{
    see \code{\link{xyplot}}.
  }
  \item{\dots}{ passed to \code{\link{panel.densityplot}} and/or
    \code{\link{panel.dotplot}}. }
}
\details{
  In the case of mixed numeric and categorical variables,
  the trellis objects from \code{dotplot()} and \code{densityplot()}
  are merged.
}
\value{
  a trellis object.
}
\author{ Felix Andrews \email{felix@nfrac.org} }
\seealso{ \code{\link{panel.dotplot}},
  \code{\link{panel.densityplot}} }
\examples{
enviro <- environmental
## make an ordered factor (so it will not be reordered)
enviro$smell <- cut(enviro$ozone, breaks = c(0, 30, 50, Inf),
    labels = c("ok", "hmmm", "yuck"), ordered = TRUE)
marginal.plot(enviro)

## using groups
enviro$is.windy <- factor(enviro$wind > 10,
    levels = c(TRUE, FALSE), labels = c("windy", "calm"))
marginal.plot(enviro[,1:5], data = enviro, groups = is.windy,
    auto.key = list(lines = TRUE))

## support for tables
marginal.plot(Titanic)
## table with groups
marginal.plot(~ Titanic, data = Titanic, groups = Survived,
    type = "b", auto.key = list(title = "Survived?"))
}
\keyword{ hplot }
