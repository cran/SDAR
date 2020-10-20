\name{summary}
\docType{methods}
\alias{summary}
\alias{summary.strata}
\title{summarize \link{strata} class objects}
\description{
The summary method displays standard information about a \link{strata} class object.
It displays a synopsis of the content in the strata object, including total number of layers,
thickness of the study section, and number of layers by lithology type. Once the stratigraphy
data are loaded into R, and validated as a \link{strata} valid object, the users are able
to explore stratigraphic information quantitatively (e.g., summarizing it by lithology type or grain size).}
\usage{
\method{summary}{strata}(object, grain.size = FALSE, \dots)
}
\arguments{
  \item{object}{an object of \link{strata} class for which a summary is desired. See \link{strata} class
       for details about the requirements of this object.}
  \item{grain.size}{default \code{\link[base:logical]{FALSE}}; a logical value indicating whether the data 
       should be summarized by grain size.}
  \item{...}{additional arguments affecting the summary produced.}
}
\value{When \code{\link[base]{summary}} function is executed over a \link{strata} object using the default options,
  the returned values are summarized by lithology; they include (i) total number of layers, (ii) total thickness of
  the section or borehole, (iii) thickness of covered intervals, (iv) thickness of each lithology expressed in percentage,
  and (v) total number of layers by lithology type. When the \emph{grain.size} argument is setting to 
  \code{\link[base:logical]{TRUE}}, summary details by grain size are included in the results.
}
\author{ John Ortiz }
\examples{

data(saltarin_beds)
saltarin_val <- strata(saltarin_beds, datum="top")
summary(saltarin_val)
summary(saltarin_val, grain.size=TRUE)
}
\keyword{methods}
