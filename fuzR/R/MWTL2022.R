#' Macrozoobenthic data set of the Dutch sector of the North Sea
#'
#' @format A list with the following components:
#' \describe{
#'   \item{\code{tabR}}{sampling stations x habitat descriptors data frame. From left to right, the variables are
#'   water depth, bottom water current speed, bottom wave energy, water stratification, sediment type,
#'   benthic primary productivity, particulate organic matter, particulate organic carbon. See Table S1
#'   in Beauchard et al. (2022) for detailed description. The three first variable, initially quantitative,
#'   were converted into factors to circumvent non-linear constraints.}
#'   \item{\code{tabL.ind.complete}}{sampling stations x taxa data frame. Faunal abundance is quantified
#'   in number of individual organisms per square meter.}
#'   \item{\code{tabL.biom.complete}}{sampling stations x taxa data frame. Faunal abundance is quantified
#'   in g AFDM per square meter.}
#'   \item{\code{tabL.ind}}{sampling stations x taxa data frame. Faunal abundance is quantified
#'   in number of individual organisms per square meter. Only taxa documented for trait are considered.}
#'   \item{\code{tabL.biom}}{sampling stations x taxa data frame. Faunal abundance is quantified
#'   in g AFDM per square meter. Only taxa documented for trait are considered.}
#'   \item{\code{tabQ}}{taxa x traits modalities data frame. Traits as qualitative variables (factors).}
#'   \item{\code{geo}}{sampling stations x geographic and geomorphological descriptors data frame.
#'   lon and lat as longitude and latitude, respectively. From BPI02 to BPI800,
#'   bathymetric position indices from 0.355 to 142 km of radius around the sampling station.}
#'   \item{\code{hab}}{factor identifying two habitats: "HD" for high hydrodynamics
#'   and "LD" for low hydrodynamics.}
#'   \item{\code{bat}}{Coarse bathymetric raster from the area.}
#' }
#'
#' @import raster
#'
#' @details
#' The first four objects of the data set are matched to each other by sampling stations.
#'
#' @usage
#' data(MWTL2022)
#'
#' @seealso
#' For a detailed application example, see
#' \code{vignette("8 Case study on life-history strategies in the North Sea", package = "fuzR").}
#'
#' @references
#' Beauchard O, Mestdagh S, Koop L, Ysebaert T, Herman PMJ (2022) Benthic synecology in a soft sediment shelf:
#' habitat contrasts and assembly rules of life strategies. Marine Ecology Progress Series 682:31–50
#'
#' Beauchard O, Soetaert K (2026) MacroTraits: a global trait data and information system for marine benthic ecology
"MWTL2022"
