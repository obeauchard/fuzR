#' Creation of trait and modality labels
#'
#' \code{fuz.lab} creates trait and modality labels with associated identifiers that are linked
#' to an existing fuzzy trait data frame.
#'
#' @param tab data frame containing fuzzy traits.
#' @param blo vector of modality counts. Its length equals the number of traits.
#' @param traits vector of character strings as trait labels.
#' @param modalities vector of character strings as trait modality labels.
#'
#' @return Returns a list of two data frames:
#' \describe{
#'   \item{\code{tab}}{A data frame containing trait data}
#'   \item{\code{lab}}{A data frame containing labels}
#' }
#'
#' @examples
#' ### Creating a fuzzy synthetic trait
#' data(MacroTraits)
#' unique(MacroTraits$labels[c("Trait.ID", "Trait")])
#'
#' ### Biodeposition
#' w <- fuz.tab(which.traits = c(23, 40))
#' list.sco <- list(c(0.0005, 0.005, 0.05, 0.5, 5, 15),
#'                  c(0, 1, 0, 0, 0, 0))
#' biodep <- fuz.comb(tab = w$tab, lab = w$lab,
#'                    which.traits = 1:2, list.sco = list.sco)
#' ### biodep clearly exhibits outliers
#' hist(biodep)
#' ### Convert biodep into a fuzzy trait
#' biodep.fuz <- fuz.conv(biodep, scan = FALSE,
#'                        interv.bound = c(0, 0.025, 0.1),
#'                        row.names = names(biodep))
#' ### Create labels
#' biodep.fuz <- fuz.lab(tab = biodep.fuz, blo = 4,
#'                       traits = "Biodeposition",
#'                       modalities = c("Null", "Low", "Intermediate", "High"))
#' head(biodep.fuz$tab)
#' biodep.fuz$lab
#'
#' @export

fuz.lab <- function(tab, blo,
                    traits, modalities){
  w1 <- paste("T", rep(1:length(blo), blo), sep = "")
  w2 <- numeric()
  for(i in 1:length(blo)){
    w <- paste(".M", 1:blo[i], sep = "")
    w2 <- c(w2, w)
  }
  Column.name <- paste(w1, w2, sep = "")
  Trait.ID <- rep(1:length(blo), blo)
  Modality.ID <- numeric()
  for(i in 1:length(blo)){
    w <- 1:blo[i]
    Modality.ID <- c(Modality.ID, w)
  }
  Trait <- rep(traits, blo)
  Modality <- modalities
  colnames(tab) <- Column.name
  lab <- data.frame(Column.name, Trait.ID,
                    Modality.ID, Trait, Modality)
  list(tab = tab, lab = lab)
}
