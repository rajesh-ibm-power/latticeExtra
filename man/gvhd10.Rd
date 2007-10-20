\name{gvhd10}
\alias{gvhd10}
\docType{data}
\title{ Flow cytometry data from five samples from a patient  }
\description{
  Flow cytometry data from blood samples taken from a Leukemia patient
  before and after allogenic bone marrow transplant.  The data spans
  five visits.
}
\usage{data(gvhd10)}
\format{
  A data frame with 113896 observations on the following 8 variables.
  \describe{
    \item{\code{FSC.H}}{ forward scatter height values}
    \item{\code{SSC.H}}{ side scatter height values }
    \item{\code{FL1.H}}{ intensity (height) in the FL1 channel }
    \item{\code{FL2.H}}{ intensity (height) in the FL2 channel }
    \item{\code{FL3.H}}{ intensity (height) in the FL3 channel }
    \item{\code{FL2.A}}{ intensity (area) in the FL2 channel }
    \item{\code{FL4.H}}{ intensity (height) in the FL4 channel }
    \item{\code{Days}}{a factor with levels \code{-6} \code{0} \code{6} \code{13} \code{20} \code{27} \code{34}}
  }
}
\source{
  \url{http://www.ficcs.org/software.html#Data_Files}
}
\references{

  Brinkman, R.R., et al. (2007).  High-Content Flow Cytometry and
  Temporal Data Analysis for Defining a
  Cellular Signature of Graft-Versus-Host Disease.
  \emph{Biology of Blood and Marrow Transplantation} \bold{13--6}

}
\keyword{datasets}
