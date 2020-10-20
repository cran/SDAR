\name{saltarin_log}
\alias{saltarin_log}
\docType{data}
\title{SaltarinST-1A\cr
       dataset (well-log)}
\description{Gamma ray log of Saltarin ST 1A.
}
\usage{
data(saltarin_log)
}
\format{The \emph{saltarin_log} dataset provided in this SDAR version comprising the gamma-ray values along of 671 meters of the Saltarin 1A borehole; it is a \link{data.frame} object with 7 (columns). 
\describe{
    \item{DEPT}{numeric; stratigraphic position where the gamma ray was measured, in meters.}
    \item{GRTH}{numeric; measured gamma ray intensity, API units.}              
   }
}
\keyword{datasets}
\examples{
data(saltarin_log)
names(saltarin_log)
}
