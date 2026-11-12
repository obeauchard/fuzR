#' Global biological trait data base on the marine macrozoobenthos
#'
#' @format A list with the following components:
#' \describe{
#'   \item{\code{taxonomy}}{data frame containing taxonomic levels as variables.}
#'   \item{\code{traits}}{long-format data frame containing biological information.}
#'   \item{\code{labels}}{data frame containing trait and modality labels.}
#'   \item{\code{distributions}}{data frame containing species biogeographic records.}
#'   \item{\code{references}}{data frame containing indexed bibliographical sources.}
#' }
#'
#' @details
#' The data base is structured as a relational system where
#' \code{taxonomy}, \code{traits} and \code{distributions} are linked via \code{Species}.
#' \code{traits} and \code{labels} via the combination of \code{Trait.ID} and \code{Modality.ID}.
#' \code{traits}, \code{distributions} and \code{references} via \code{Reference.ID}.
#'
#' @usage
#' data(MacroTraits)
#'
#' @references
#' Beauchard O, Soetaert K (2026) MacroTraits: a global trait data and information system for marine benthic ecology.
"MacroTraits"
