\name{gplotArgs.data.frame}
\alias{gplotArgs.data.frame}
\alias{gplotArgs<-.data.frame}
\title{Grouped Display Arguments from a Data Frame}
\description{
  Extract arguments suitable for gplot from a data.frame with
  appropriate meta-data
}
\synopsis{
gplotArgs.data.frame(x, display.formula, outer = FALSE, inner = FALSE, 
                     groups = NULL, \dots, subset = TRUE)
}
\usage{
\method{gplotArgs}{data.frame}(x, display.formula,
          outer = FALSE, inner = FALSE, \dots)
\method{gplotArgs}{data.frame}(x) <- value
}
\arguments{
  \item{x}{ an object of class ``data.frame'' }
  \item{display.formula}{ a Trellis display formula }
  \item{outer}{ logical or one-sided formula}
  \item{inner}{ logical or one-sided formula}
  \item{\dots}{ extra arguments, passed on as appropriate  }
  \item{value}{

    a list containing named components, to be stored as special
    attributes of \code{x}, including but not limited to

    \describe{
      \item{formula:}{
        a model formula with a response, covariate and grouping
        variable, i.e. of the form \code{y ~ x | g}, where \code{x}
        could be \code{1}
      }

      \item{order.groups:}{
        logical, defaults to \code{TRUE}, whether levels of \code{g}
        will be reordered for the plot
      }
      \item{FUN:}{ a function of \code{x} used to reorder levels of
        \code{g}, applied to subsets of \code{x} determined by \code{g}
      }
      \item{outer:}{ one-sided formula, defaults to \code{NULL}}
      \item{inner:}{one-sided formula, defaults to \code{NULL}}
      \item{labels:}{
        list of character strings (or expressions?) with named
        comonents, specifying variable names used in plot labels.  The
        component names must be variables in the data frame.  Not all
        variables need be present (if not, the variable names will be
        used for labels).
      }
      \item{units:}{
        list, similar to \code{labels}, but specifying the units of
        measurement (if present, used in constructing labels)
      }
    }

    These are modeled on the \code{groupedData} function in package
    \code{nlme}, and are stored as meta-data in the data frame
    \code{x}.  The components listed above are special in the sense that
    they are used by the corresponding \code{gplotArgs} method to
    contruct a suitable list as descibed in
    \code{\link{gplotArgs.default}}.  Any other components in
    \code{value} can be used to override the values thus constructed,
    including \code{display.formula}.
  }
}
\details{
  The \code{gplotArgs} method for data frames constructs a suitable list
  as descibed in \code{\link{gplotArgs.default}}.  The
  \code{display.formula} argument can be used to specify the Trellis
  formula used (usually, it will be constructed from the meta-data).

  For the role of \code{outer} and more details, consult \code{nlme}
  documentation for now.
}

\value{
  \code{gplotArgs} should return a list.
}
\author{ Deepayan Sarkar \email{deepayan@stat.wisc.edu}}
\seealso{
  \code{\link{gplot}}
}
\examples{
gplotArgs(iris) <-
    list(formula = Sepal.Length ~ 1 | Species,
         order.groups = TRUE,
         labels = list(Sepal.Length = "Sepal\nLength",
         Sepal.Width = "Sepal\nWidth"))
gplot(iris)
gplot(iris, display.formula = Sepal.Length ~ Sepal.Width | Species)
}
\keyword{dplot}
