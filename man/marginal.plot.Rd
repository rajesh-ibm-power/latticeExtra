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
              reorder = TRUE,
              plot.points = FALSE,
              ref = TRUE,
              origin = 0,
              xlab = NULL, ylab = NULL,
              type = c("p", if (is.null(groups)) "h"),
              ...,
              subset = TRUE,
              as.table = TRUE,
              subscripts = TRUE,
              par.settings = simpleTheme(cex = 0.6),
              default.scales = list(
                x = list(relation = "free", abbreviate = TRUE,
                  rot = 60, cex = 0.5, tick.number = 3),
                y = list(relation = "free", draw = FALSE)))
}
\arguments{
  \item{x}{ a data frame, or a formula of which the first term is a data
    frame, or something that can be coerced to a data frame. }
  \item{data}{ an optional data source in which groups and subset may be
    be evaluated. }
  \item{groups}{ term, to be evaluated in \code{data}, that is used as a
    grouping variable. }
  \item{reorder}{ whether to reorder factor variables by frequency. }
  \item{subset}{ data subset expression, evaluated in \code{data}. }
  \item{plot.points, ref}{ passed to \code{panel.densityplot}. }
  \item{origin, type}{ passed to \code{panel.dotplot}. }
  \item{xlab, ylab, as.table,
    subscripts, par.settings, default.scales}{
    see \code{\link[lattice]{xyplot}}.
  }
  \item{\dots}{ passed to \code{\link[lattice]{panel.densityplot}} and
    \code{\link[lattice]{panel.dotplot}}. }
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
\seealso{ \code{\link[lattice]{panel.dotplot}},
  \code{\link[lattice]{panel.densityplot}} }
\examples{
enviro <- environmental
## make an ordered factor (so it will not be reordered)
enviro$smell <- cut(enviro$ozone, breaks = c(0, 30, 50, Inf),
    labels = c("ok", "hmmm", "yuck"), ordered = TRUE)
marginal.plot(enviro)

## using groups:
enviro$is.windy <- factor(enviro$wind > 10,
    levels = c(TRUE, FALSE), labels = c("windy", "calm"))
marginal.plot(enviro[,1:5], data = enviro, groups = is.windy,
    auto.key = list(lines = TRUE))
}
\keyword{ hplot }
