\name{ancestry}
\alias{ancestry}
\docType{data}
\title{ Modal ancestry by County according to US 2000 Census }
\description{
  This data set records the population and the three most frequently
  reported ancestries by US county, according to the 2000 census.
}
\usage{data(ancestry)}
\format{
  A data frame with 3219 observations on the following 5 variables.
  \describe{
    \item{\code{county}}{ A factor. An attempt has been made to make the
      levels look similar to the county names used in the \code{maps}
      package.  }
    \item{\code{population}}{a numeric vector}
    \item{\code{top}}{a character vector}
    \item{\code{second}}{a character vector}
    \item{\code{third}}{a character vector}
  }
}
\source{

  U.S. Census Bureau.  The ancestry data were extracted from Summary
  File 3, available from
  
  \url{http://www.census.gov/census2000/sumfile3.html}
  
  which is based on the \sQuote{long form} questionnaire (asked to 1 in
  6 households surveyed).
}
\seealso{
  \code{\link{mapplot}}, for examples.
}
\references{
  \url{http://www.census.gov/prod/cen2000/doc/sf3.pdf}
}
\keyword{datasets}
