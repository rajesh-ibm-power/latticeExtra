\name{USCancerRates}
\alias{USCancerRates}
\docType{data}
\title{ Rate of Death Due to Cancer in US Counties }
\description{

  This data set records the annual rates of death (1999-2003) due to
  cancer by sex in US counties.

}
\usage{data(USCancerRates)}
\format{
  A data frame with 3041 observations on the following 8 variables.
  \describe{
    \item{\code{rate.male}}{a numeric vector, giving rate of death per
      100,000 due to cancer among males}
    \item{\code{LCL95.male}}{a 95\% lower confidence limit for \code{rate.male} }
    \item{\code{UCL95.male}}{a 95\% upper confidence limit for \code{rate.male} }
    \item{\code{rate.female}}{a numeric vector, giving rate of death per
      100,000 due to cancer among females}
    \item{\code{LCL95.female}}{a 95\% lower confidence limit for \code{rate.female}}
    \item{\code{UCL95.female}}{a 95\% upper confidence limit for \code{rate.female}}
    \item{\code{state}}{ a factor with levels giving name of US state }
    \item{\code{county}}{ a character vector giving county names, in a
      format similar to that used for county map boundaries in the
      \code{maps} package. }
  }
}
\details{
  See the scripts directory for details of data preprocessing steps.

  From the website: Death data provided by the National Vital Statistics
  System public use data file.  Death rates calculated by the National
  Cancer Institute using SEER*Stat.  Death rates are age-adjusted to the
  2000 US standard population
  [\url{http://www.seer.cancer.gov/stdpopulations/stdpop.19ages.html}].
  Population counts for denominators are based on Census populations as
  modified by NCI.
  
}
\source{
  \url{http://statecancerprofiles.cancer.gov/}
}
\examples{
data(USCancerRates)
}
\keyword{datasets}
