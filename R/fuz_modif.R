#' Fuzzy-coded trait modification
#'
#' \code{fuz.modif} reshapes a fuzzy-coded trait within an existing taxa by trait modalities cross table
#' according to desired changes in number of modalities and associated labels. It simplifies a trait
#' when some of its modalities are either not relevant or their rarity not important.
#'
#' @param tab data frame containing fuzzy traits.
#' @param lab data frame containing trait and modality identifiers and full labels.
#' @param which.trait integer identifying the trait to be modified.
#' @param merge vector of integers corresponding respectively to the modalities of the trait to be modified.
#' Modalities that attributed the same value will merge into a same modality in the modified version of the trait.
#' @param fun function used when merging scores from different modalities. \code{max} by default.
#' @param mod vector of modality labels. Its length must equal \code{max(merge)}.
#' @param column.name column names in \code{tab} as a variable in \code{lab}.
#' @param traitID trait identifier variable in \code{lab}.
#' @param modalityID modality identifier variable in \code{lab}.
#' @param trait full trait label variable in \code{lab}.
#'
#' @return Returns a list of two data frames:
#' \describe{
#'   \item{\code{tab}}{A data frame containing trait data}
#'   \item{\code{lab}}{A data frame containing labels}
#' }
#'
#' @examples
#' data(MacroTraits)
#' ### Create a trait data cross table
#' unique(MacroTraits$labels[c("Trait.ID", "Trait")])
#' w <- fuz.tab(which.traits = c(37, 33, 34, 36))
#' indic <- w$lab$Trait.ID
#' ### Visualising the table
#' head(w$tab)[indic %in% 1:2]
#' ### Visualising the labels
#' w$lab[indic %in% 1:2,]
#' ### Modify the trait "Endo-bioconstruction type"
#' w <- fuz.modif(tab = w$tab, lab = w$lab, which.trait = 2,
#'                merge = c(1, 2, 2, 1, 2, 3, 2, 3),
#'                mod = c("None", "Blind-ended", "Open-ended"))
#' indic <- w$lab$Trait.ID
#' ### The new version of the trait
#' w$lab[indic == 2,]
#' head(w$tab)[indic %in% 1:2]
#'
#' @export

fuz.modif <- function(tab, lab, which.trait, merge, fun = max, mod = NULL,
                      column.name = "Column.name", traitID = "Trait.ID",
                      modalityID = "Modality.ID", trait = "Trait"){
  tr <- tab[lab[,traitID] == which.trait]
  tr <- apply(tr, 1, function(x) tapply(x, factor(merge), fun))
  tr <- data.frame(t(tr))
  if(is.null(mod)){
    w <- length(unique(merge))
    mod <- paste("M", 1:w, sep = "")
  }
  w <- lab[,column.name][lab[,traitID] == which.trait][1:length(mod)]
  colnames(tr) <- w
  tab <- tab[lab[,traitID] != which.trait]
  tab <- cbind(tab, tr)
  df <- data.frame(Column.name = w,
                   Trait.ID = rep(which.trait, length(mod)),
                   Modality.ID = 1:length(mod),
                   Trait = rep(unique(lab[,trait])[which.trait], length(mod)),
                   Modality = mod)
  lab <- lab[lab[,traitID] != which.trait,]
  lab <- rbind(lab, df)
  tab <- tab[order(lab[,traitID], lab[,modalityID])]
  lab <- lab[order(lab[,traitID], lab[,modalityID]),]
  list(tab = tab, lab = lab)
}
