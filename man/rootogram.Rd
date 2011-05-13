\name{rootogram}
\alias{rootogram}
\alias{rootogram.formula}
\alias{panel.rootogram}
\alias{prepanel.rootogram}
\title{Trellis Displays of Tukey's Hanging Rootograms}
\description{
  Displays hanging rootograms. 
}
\usage{
rootogram(x, \dots)

\method{rootogram}{formula}(x, data = parent.frame(),
          ylab = expression(sqrt(P(X == x))),
          prepanel = prepanel.rootogram,
          panel = panel.rootogram,
          ...)

prepanel.rootogram(x, y = table(x),
                   dfun = NULL,
                   transformation = sqrt,
                   hang = TRUE,
                   \dots)

panel.rootogram(x, y = table(x),
                dfun = NULL,
                col = plot.line$col,
                lty = plot.line$lty,
                lwd = plot.line$lwd,
                alpha = plot.line$alpha,
                transformation = sqrt,
                hang = TRUE,
                \dots)

}
\arguments{
  \item{x, y}{ For \code{rootogram}, \code{x} is the object on which
    method dispatch is carried out.  For the \code{"formula"} method,
    \code{x} is a formula describing the form of conditioning plot.  The
    formula can be either of the form \code{~x} or of the form
    \code{y~x}.  In the first case, \code{x} is assumed to be a vector
    of raw observations, and an observed frequency distribution is
    computed from it.  In the second case, \code{x} is assumed to be
    unique values and \code{y} the corresponding frequencies.  In either
    case, further conditioning variables are allowed.

    A similar interpretation holds for \code{x} and \code{y} in
    \code{prepanel.rootogram} and \code{panel.rootogram}.

    Note that the data are assumed to arise from a discrete distribution
    with some probability mass function.  See details below.
  }

  \item{data}{ For the \code{"formula"} method, a data frame containing
    values for any variables in the formula, as well as those in
    \code{groups} and \code{subset} if applicable (\code{groups} is
    currently ignored by the default panel function).  By default the
    environment where the function was called from is used.  }

  \item{dfun}{ a probability mass function, to be evaluated at unique x
    values }

  \item{prepanel, panel}{ panel and prepanel function used to create the
    display.  }

  \item{ylab}{ the y-axis label; typically a character string or an
    expression. }

  \item{col, lty, lwd, alpha}{ graphical parameters }
  
  \item{transformation}{ a vectorized function.  Relative frequencies
    (observed) and theoretical probabilities (\code{dfun}) are
    transformed by this function before being plotted. }

  \item{hang}{logical, whether lines representing observed relative
    freuqncies should \dQuote{hang} from the curve representing the
    theoretical probabilities. }

  \item{\dots}{ extra arguments, passed on as appropriate.  Standard
    lattice arguments as well as arguments to \code{panel.rootogram}
    can be supplied directly in the high level \code{rootogram} call.
  }
}

\details{

  This function implements Tukey's hanging rootograms.  As implemented,
  \code{rootogram} assumes that the data arise from a discrete
  distribution (either supplied in raw form, when \code{y} is
  unspecified, or in terms of the frequency distribution) with some
  unknown probability mass function (p.m.f.).  The purpose of the plot
  is to check whether the supplied theoretical p.m.f. \code{dfun} is a
  reasonable fit for the data.

  It is reasonable to consider rootograms for continuous data by
  discretizing it (similar to a histogram), but this must be done by the
  user before calling \code{rootogram}.  An example is given below.

  Also consider the \code{rootogram} function in the \code{vcd} package,
  especially if the number of unique values is small.

}

\value{

  \code{rootogram} produces an object of class \code{"trellis"}.  The
  \code{update} method can be used to update components of the object and
  the \code{print} method (usually called by default) will plot it on an
  appropriate plotting device.

}

\references{

  John W. Tukey (1972) Some graphic and semi-graphic displays. In
  T. A. Bancroft (Ed) \emph{Statistical Papers in Honor of George
  W. Snedecor}, pp. 293--316.  Available online at
  \url{http://www.edwardtufte.com/tufte/tukey}

}

\author{ Deepayan Sarkar \email{deepayan.sarkar@gmail.com}}
\seealso{
  \code{\link{xyplot}}
}

\examples{

library(lattice)

x <- rpois(1000, lambda = 50)

p <- rootogram(~x, dfun = function(x) dpois(x, lambda = 50))
p

lambdav <- c(30, 40, 50, 60, 70)

update(p[rep(1, length(lambdav))],
       aspect = "xy",
       panel = function(x, ...) {
           panel.rootogram(x,
                           dfun = function(x)
                           dpois(x, lambda = lambdav[panel.number()]))
       })


lambdav <- c(46, 48, 50, 52, 54)

update(p[rep(1, length(lambdav))],
       aspect = "xy",
       prepanel = function(x, ...) {
           tmp <-
               lapply(lambdav,
                      function(lambda) {
                          prepanel.rootogram(x,
                                             dfun = function(x)
                                             dpois(x, lambda = lambda))
                      })
           list(xlim = range(sapply(tmp, "[[", "xlim")),
                ylim = range(sapply(tmp, "[[", "ylim")),
                dx = do.call("c", lapply(tmp, "[[", "dx")),
                dy = do.call("c", lapply(tmp, "[[", "dy")))
       },
       panel = function(x, ...) {
           panel.rootogram(x,
                           dfun = function(x)
                           dpois(x, lambda = lambdav[panel.number()]))
           grid::grid.text(bquote(Poisson(lambda == .(foo)),
                                  where = list(foo = lambdav[panel.number()])),
                           y = 0.15,
                           gp = grid::gpar(cex = 1.5))
       },
       xlab = "",
       sub = "Random sample from Poisson(50)")


## Example using continuous data

xnorm <- rnorm(1000)

## 'discretize' by binning and replacing data by bin midpoints

h <- hist(xnorm, plot = FALSE)

## Option 1: Assume bin probabilities proportional to dnorm()

norm.factor <- sum(dnorm(h$mids, mean(xnorm), sd(xnorm)))

rootogram(counts ~ mids, data = h,
          dfun = function(x) {
              dnorm(x, mean(xnorm), sd(xnorm)) / norm.factor
          })

## Option 2: Compute probabilities explicitly using pnorm()

pdisc <- diff(pnorm(h$breaks, mean = mean(xnorm), sd = sd(xnorm)))
pdisc <- pdisc / sum(pdisc)

rootogram(counts ~ mids, data = h,
          dfun = function(x) {
              f <- factor(x, levels = h$mids)
              pdisc[f]
          })

}

\keyword{dplot}
