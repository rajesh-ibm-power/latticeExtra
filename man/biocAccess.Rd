\name{biocAccess}
\alias{biocAccess}
\docType{data}
\title{ Hourly access attempts to Bioconductor website }
\description{
  This data set records the hourly number of access attempts to
  the Bioconductor website (\url{http://www.bioconductor.org}) during
  January through May of 2007.  The counts are essentially an
  aggregation of the number of entries in the access log.
}
\usage{data(biocAccess)}
\format{
  A data frame with 3623 observations on the following 7 variables.
  \describe{
    \item{\code{counts}}{ the number of access attempts}
    \item{\code{day}}{ the day of the month }
    \item{\code{month}}{a factor with levels \code{Jan}, \code{Feb},
      \dots, \code{Dec}}
    \item{\code{year}}{ the year (all 2007) }
    \item{\code{hour}}{ hour of the day, a numeric vector}
    \item{\code{weekday}}{ a factor with levels \code{Monday},
      \code{Tuesday}, \dots, \code{Sunday}}
    \item{\code{time}}{a POSIXt representation of the start of the hour}
  }
}
\examples{
data(biocAccess)
xyplot(stl(ts(biocAccess$counts[1:(24 * 30)], frequency = 24), "periodic"))
}
\keyword{datasets}
