\name{SeatacWeather}
\alias{SeatacWeather}
\docType{data}
\title{ Daily Rainfall and Temperature at the Seattle-Tacoma Airport }
\description{
  Daily Rainfall and Temperature at the Seattle-Tacoma Airport between 
  January through March of 2007.
}
\usage{data(SeatacWeather)}
\format{
  A data frame with 90 observations on the following 14 variables.
  \describe{
    \item{\code{month}}{a factor with levels \code{January},
      \code{February}, and \code{March}}
    \item{\code{day}}{ day of the month }
    \item{\code{year}}{ year, all 2007 }
    \item{\code{max.temp}}{ maximum temperature (Fahrenheit) }
    \item{\code{record.max}}{ record maximum temperature }
    \item{\code{normal.max}}{ normal maximum temperature }
    \item{\code{min.temp}}{ minimum temperature }
    \item{\code{record.min}}{ record minimum temperature }
    \item{\code{normal.min}}{ normal minimum temperature }
    \item{\code{precip}}{ precipitation (inches) }
    \item{\code{record.precip}}{ record precipitation }
    \item{\code{normal.precip}}{ normal precipitation }
    \item{\code{time.max}}{ time of maximum temperature }
    \item{\code{time.min}}{ time of minimum temperature }
  }
}
\details{
  The time of minimum and maximum temperatures should be interpreted as
  follows: the least two significant digits denote minutes (out of 60)
  and the next two significant digits denote hout (out of 24).
}
\source{
  \url{http://www.atmos.washington.edu/cgi-bin/list_climate.cgi?clisea}
}
\keyword{datasets}
