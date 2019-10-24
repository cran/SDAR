\name{read.LAS}
\alias{read.LAS}
\title{read.LAS files}
\usage{
read.LAS(filePath, repl.null = FALSE, writecsv=FALSE)
}
\arguments{
  \item{filePath}{the name of the .LAS file which the data are to be read. If \code{filePath} does not contain an
  \emph{absolute} path, the file name is \emph{relative} to the current working directory.
  \code{filePath} can also be a complete URL.
  }
  \item{repl.null}{logical, default \code{\link[base:logical]{FALSE}}. It refers to null values; commonly used null values
  in .LAS files are -9999, -999.25 and -9999.25.
  If this argument is set to \code{\link[base:logical]{TRUE}}, the null values will be replaced by missing values (\bold{NA}).
  }
  \item{writecsv}{logical, default \code{\link[base:logical]{FALSE}}. If \code{\link[base:logical]{TRUE}} the data \emph{~A (ASCII Log Data)} will be saved in a CSV format. Note that the file is written to your working directory; row and column names are included. See default options of \code{\link[utils]{write.csv}} function.
  }
}
\description{ 
\href{https://en.wikipedia.org/wiki/Log_ASCII_Standard}{Log ASCII Standard} (LAS) is a standard file-format common 
in the oil-and-gas and water-well industries to store well log information. This function is designed to read Log ASCII 
Standard (LAS) files; the objective of this function is to omit the information in the header of the LAS file, and 
extract only the data (curve information \emph{~A "ASCII Log Data"}) to keep it into R data structure 
\code{\link[base]{data.frame}}. Also, it includes the option of write the data table (curve information) as CSV file.
\emph{WARNING: This method is very much in an alpha stage.
Expect it to change.}
}
\author{John Ortiz}
\note{
\link{read.LAS} function included in SDAR packages is not very mature yet; it is an experimental version.
We are working on making it compatible with versions 1.2, 2.0 and 3.0 of the LAS file specification published by the \href{http://www.cwls.org/las/}{Canadian Well Logging Society}(CWLS).\cr
This is NOT a function to read LAS file formats used for Remote Sensing Data (also called LAS files); there is a package on CRAN called \CRANpkg{rlas} to read and write this format.\cr
The source of the file '1044907529.las' Well 13-T (API: 15-003-25233) is the \href{http://www.kgs.ku.edu/}{Kansas Geological Survey website}. All Rights Reserved.
}
\examples{
 fpath <- system.file("demo_data_entry", "1044907529.las", package = "SDAR")
 data_logs <- read.LAS(fpath, repl.null=TRUE)
 names(data_logs)
 GR_SDAR_format <- data_logs[,c(1, 4)]
}