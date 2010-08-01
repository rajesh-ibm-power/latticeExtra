\name{ecdfplot}
\alias{ecdfplot}
\alias{ecdfplot.formula}
\alias{ecdfplot.numeric}
\alias{panel.ecdfplot}
\alias{prepanel.ecdfplot}
\title{Trellis Displays of Empirical CDF}
\description{
  Conditional displays of Empirical Cumulative Distribution Functions
}

\usage{
ecdfplot(x, data, \dots)

\method{ecdfplot}{formula}(x, data,
         prepanel = "prepanel.ecdfplot", 
         panel = "panel.ecdfplot",
         ylab,
         \dots)
\method{ecdfplot}{numeric}(x, data = NULL, xlab, \dots)

prepanel.ecdfplot(x, f.value = NULL, \dots)

panel.ecdfplot(x, f.value = NULL, type = "s",
               groups = NULL, qtype = 7,
               ref = TRUE,
               \dots)

}
\arguments{
  \item{x}{ For \code{ecdfplot}, \code{x} is the object on which
    method dispatch is carried out.  For the \code{"formula"} method,
    \code{x} is a formula describing the form of conditioning plot, and
    has to be of the form \code{~x}, where \code{x} is assumed to be a
    numeric vector.  Further conditioning variables are allowed as
    usual.

    A similar interpretation holds for \code{x} in the \code{"numeric"}
    method as well as \code{prepanel.ecdfplot} and
    \code{panel.ecdfplot}.
  }

  \item{data}{ For the \code{"formula"} method, a data frame containing
    values for any variables in the formula, as well as those in
    \code{groups} and \code{subset} if applicable. }

  \item{prepanel, panel}{ panel and prepanel function used to create the
    display.  }

  \item{xlab, ylab}{ axis labels; typically a character string or an
    expression. }

  \item{groups}{ a grouing variable of the same length as \code{x}.  If
    specified, ECDF plots are computed for each subset defined by unique
    values of \code{groups} and the resulting functions superposed
    within each panel.
  }
  \item{f.value, qtype}{ Defines how quantiles are calculated. See
    \code{\link{panel.qqmath}}. }
  \item{ref}{ logical, whether a reference line should be drawn at 0 and
    1}
  \item{type}{ how the plot is rendered; see
    \code{\link{panel.xyplot}} }

  \item{\dots}{ extra arguments, passed on as appropriate.  Standard
    lattice arguments as well as arguments to \code{panel.ecdfplot}
    can be supplied directly in the high level \code{ecdfplot} call.
  }
}

\value{
  \code{ecdfplot} produces an object of class \code{"trellis"}. The
  \code{update} method can be used to update components of the object and
  the \code{print} method (usually called by default) will plot it on an
  appropriate plotting device.

}
\author{ Deepayan Sarkar \email{deepayan.sarkar@r-project.org}}
\seealso{
  \code{\link{qqmath}} for Quantile plots which are
  more generally useful, especially when comparing with a theoretical
  distribution other than uniform.  An ECDF plot is essentially a
  transposed version (i.e., with axes switched) of a uniform quantile
  plot.
}

\examples{

data(singer, package = "lattice")
ecdfplot(~height | voice.part, data = singer)

}

\keyword{dplot}
