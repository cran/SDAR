\name{saltarin_beds}
\alias{saltarin_beds}
\docType{data}
\title{Lithological description of borehole SaltarinST-1A\cr
       dataset (beds/layers)}
\description{This dataset gives a lithologic description for borehole Saltarin 1A, located in the
Llanos Basin in eastern Colombia (4.612 N, 70.495 W). The stratigraphic well Saltarin 1A drilled
671 meters of the Miocene succession of the eastern Llanos basin, corresponding to the Carbonera
(124.1 m; 407.1 ft), Leon (105.1 m; 344.8 ft), and Guayabo Formations (441.8 m; 1449.5 ft) (Bayona,
et al. 2008). The Saltarin core was described at a scale of 1:50 for identification of grain-size
trends, sedimentary structures, clast composition, thickness of lamination, bioturbation patterns,
and macrofossil identification, all of which are used for identify individual lithofacies and for
sedimentological and stratigraphic analyses.
}
\usage{
data(saltarin_beds)
}
\format{The \emph{Saltarin} dataset provided by this package is a dataset comprising the lithological description
of 686 rock layers described along of 671 meters of the Saltarin 1A borehole; it is a \link{data.frame} object
with 686 layers (rows), including thickness, composition and texture description of each layer, stored following
the suggested format by SDAR (22 columns). 
\describe{
    \item{bed_number}{numeric; it is useful to give each bed/layer a number to facilitate later reference;
          numbering begins at the stratigraphically lowest bed (Tucker 2011)}
    \item{base}{numeric; stratigraphic position measured at the bottom of each layer, in meters.}
    \item{top}{numeric; stratigraphic position measured at the top of each layer, in meters.}
    \item{rock_type}{string; rock type class following this clasification: \emph{sedimentary, volcaniclastic, or  
          covered}.}
    \item{prim_litho}{string; description of the primary (i.e. major) lithology class, (e.g., \emph{claystone, mudstone, 
          siltstone, shale, sandstone, conglomerate, breccia, limestone, coal}).}
    \item{grain_size}{string; description of the grain size dominant in layer. For siliciclastic rocks, it is based
          on the Wentworth (1922) classification system, for carbonate rocks it is based on Dunham (1962),
          and for classification of pyroclastic rocks it is based on Wentworth and Williams (1932). (e.g., \emph{clay, silt; 
          very fine sand, fine sand, medium sand, coarse sand, and very coarse sand}); see all the grain-size options
          in table 3 at SDAR data model vignettes.}
    \item{prim_litho_percent}{numeric; percent of primary lithology.}
    \item{sec_litho}{string; using same categories described for \emph{prim_litho}.}
    \item{grain_size_sec_litho}{string; description of the grain size for the secondary lithology in layer; using same categories
          described for \emph{grain_size}}
    \item{sec_litho_percent}{numeric; percent of secondary lithology.}  
    \item{base_contact}{string; description of the boundary at the base of the layer (geological contact): \emph{sharp 
          planar, gradational, faulted, covered}.}
    \item{grading}{string; description of the vertical variations in grain size; \emph{normal} (beds that
          show gradation from coarser particles at the base to finer particles at the top), \emph{inverse} (reverse or inverse grading, gradation from finer at the base to coarser at the top), (Boggs, 2011).}
    \item{grain_size_base}{string; grain size at the base of the layer using same categories described for \emph{ 
          grain_size}.}
    \item{grain_size_top}{string; grain size at the top of the layer using same categories described for \emph{
          grain_size}.}
    \item{sorting}{string; description of the degree of uniformity of grain size: \emph{very well sorted, well sorted, 
          moderately sorted, poorly sorted, very poorly sorted}.}
    \item{roundness}{string; measurement of roundness of sedimentary particles (Krumbein 1941): \emph{very angular,
          angular, sub angular, sub rounded, rounded, well rounded}.}
    \item{matrix}{string; composition of material wherein larger grains, crystals or clasts are embedded: \emph{muddy,
          sandy, ashy, micrite, not recognizable}.}
    \item{cement}{string; composition of the crystalline material precipitated around the edges of grains: \emph{
          siliceous, ferruginous, calcite, dolomite, kaolinitic, sparite, not recognizable}.}
    \item{fabric}{string; grain fabric (packing). For sedimentary rocks: \emph{clast supported, matrix supported}.}
    \item{munsell_color}{string; color description based on the Munsell chart codes from the Geological Society of
          America Rock Color Chart (Committee 1991)}
    \item{Rcolor}{string; R colors can be specified either by name (e.g col = "red") or as a hexadecimal RGB triplet (such as col = "#FFCC00").}
    \item{notes}{string; additional description in a free text format ('Comments/Remarks').}              
   }
}
\source{
  Access to the well core was provided by Alejandro Mora and Andres Fajardo of HOCOL S.A. 
  (core description: 2007)
}
\author{Lithological description: Bayona, et al. 2008 \cr
  Sedimentological and Stratigraphic Interpretation: Bayona, G. and Duarte, E.
}
\references{
Bayona, G., Valencia, A., Mora, A., Rueda, M., Ortiz, J., Montenegro, O. 2008. \emph{Estratigrafia y procedencia de las rocas del Mioceno en la parte distal de la cuenca antepais de los Llanos de Colombia.} Geologia Colombiana, \bold{33}, 23-46.

Bogs, S. 2011. \emph{Principles of Sedimentology and Stratigraphy.} Prentice Hall, fifth edition. 

Dunham, R. 1962. \emph{American Association of Petroleum Geologists Memoir. Classification of carbonate rocks according to
depositional texture.} In Ham, W.E. Classification of carbonate rocks, \bold{1}, 108-121

Krumbein, W. C. 1941. \emph{The effects of abrasion on the size, shape and roundness of rock fragments.} The Journal
          of Geology \bold{49(5)}, 482-520. 

Munsell Geological Rock-Color Chart With Genuine Munsell Color Chips. Munsell Color, 2009. 

Tucker, M. E. 2011. \emph{Sedimentary Rocks in the Field: A Practical Guide}. Geological Field Guide. Wiley

Wentworth, C. K. 1922. \emph{A scale of grade and class terms for clastic sediments}. Journal of Geology, \bold{30}, 377-392.
    
Wentworth, C. K. and Williams, H. 1932. \emph{Classification and terminology of pyroclastic rocks}. National Research
Council Bulletin, \bold{89}, 19-53.
}
\keyword{datasets}
\examples{
data(saltarin_beds)
names(saltarin_beds)
}
