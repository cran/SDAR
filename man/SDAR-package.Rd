\name{SDAR}
\alias{SDAR}
\docType{package}
\title{
SDAR: A Toolkit for Stratigraphic Data Analysis in R
}
\description{
SDAR is an analytical package designed for both plotting and facilitating the analysis
    of stratigraphic and sedimentological data. Taking advantage of the flexible plotting
    tools available in R, SDAR uses stratigraphic and sedimentological data to produce 
    detailed graphic logs for outcrop sections and borehole logs. These logs can include
    multiple features (e.g.,  bed thickness, lithology, samples, sedimentary structures,
    colors, fossil content, bioturbation index, electrical logs).

    This package includes a dataset giving a lithologic description for borehole Saltarin 1A.
    The Saltarin core was described at a scale of 1:50 for identification of grain-size trends,
    sedimentary structures, clast composition, thickness of lamination, bioturbation patterns,
    and macrofossil identification (Bayona, et al. 2008).
}

\author{
Author: John R. Ortiz [aut, cre],\cr
	      Carlos Jaramillo [aut] (author support and guidance),\cr
          Carlos Moreno [ctb].\cr
Maintainer: <John R. Ortiz <jrortizt@unal.edu.co>
}

\section{Installation and updates}{

  To install this package do:\cr

  \code{install.packages("SDAR")}
}

\section{License}{
  The releases of this package is licensed under
  GPL version 2 or newer.
}

\section{Acknowledgments}{
I would like to thank geologists from STRI, Corporacion Geologica ARES, and Instituto Colombiano
del Petroleo (ICP-ECOPETROL) for supporting the SDAR project and for giving me many exciting ideas
and much geological knowledge. Moreover, I would like to express my gratitude to my supervisor, 
Prof. Dr. Edzer Pebesma (Institute for Geoinformatics), and to my co-supervisors, Prof. 
Dr. Sebastien Castelltort (Department of Earth Sciences, University of Geneva) and Prof. Dr. Jorge
Mateu (Department of Mathematics, University Jaume I). They contributed to this project
with their valuable ideas and helpful advice. Furthermore, I am much indebted to G. Bayona, M.
Baquero, F. Moreno, C. D Apolito, A. Cardenas, and C. Montes for their valuable critical reviews.
Finally, I also acknowledge the R Core Team and its large group of R contributors for their hard
work.\cr\cr
Financial support of this research was provided by Colciencias (partly funding the master studies
of the main author), Fundacion para la Promocion de la Investigacion y la Tecnologia del Banco de la
Republica, (Colombia), and the Smithsonian Tropical Research Institute, the Anders Foundation, 
1923 Fund, and Gregory D. and Jennifer Walston Johnson.\cr\cr
The Saltarin 1A well dataset for this analysis, was provided by Alejandro Mora of **HOCOL S.A.**
}

\references{
  Bayona, G., Valencia, A., Mora, A., Rueda, M., Ortiz, Johan., Montenegro, O. 2008. \emph{Estratigrafia
  y procedencia de las rocas del Mioceno en la parte distal de la cuenca antepais de los Llanos de
  Colombia. Geologia Colombiana, \bold{33}, 23-46.
  }
}
\keyword{package}