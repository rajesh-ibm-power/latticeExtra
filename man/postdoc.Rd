\name{postdoc}
\alias{postdoc}
\docType{data}
\title{ Reasons for Taking First Postdoctoral Appointment }
\description{
  Reasons for Taking First Postdoctoral Appointment, by Field of
  Doctrate, 1997
}
\usage{data(postdoc)}
\format{
  The data set is avaliable as a two-way table of counts.  
}
\source{
  Survey of Doctorate Recipients, 1997
}
\references{
  Enhancing the Postdoctoral Experience for Scientists and Engineers:
  A Guide for Postdoctoral Scholars, Advisers, Institutions, Funding
  Organizations, and Disciplinary Societies

  \url{http://books.nap.edu/catalog.php?record_id=9831}
}
\examples{
data(postdoc)
library(lattice)
barchart(prop.table(postdoc, margin = 1),
         auto.key = TRUE, xlab = "Proportion")
}
\keyword{datasets}
