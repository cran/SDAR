\name{log_demo}
\alias{log_demo}
\docType{data}
\title{Simulated log demo borehole Saltarin 1A\cr
       dataset (gamma-ray)}
\description{this gamma-ray log is a simulated dataset generated for SDAR examples, this electrical log does not have a geological meaning".
}
\usage{
data(log_demo)
}
\format{The \emph{log_demo} dataset provided by this package is a provisional dataset comprising the gamma-ray values along of 671 meters of the Saltarin 1A borehole; it is a \link{data.frame} object
with 2 (columns), Depth and GR values. 
\describe{
    \item{Depth}{numeric; stratigraphic position where the Gamma-ray was measured, in meters.}
    \item{GR}{numeric; measured gamma ray intensity, API units.}              
   }
}
\keyword{datasets}
\examples{
data(log_demo)
names(log_demo)
}
