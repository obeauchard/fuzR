#' Fuzzy-coded trait removal
#'
#' \code{fuz.sel} selects fuzzy-coded traits within an existing taxa by trait modalities cross table and adjust
#' accordingling trait and modality labels. Useful when some traits are not relevant or create noise
#' within a multidimensional pattern.
#'
#' @param tab data frame containing fuzzy-coded traits.
#' @param lab data frame containing trait and modality identifiers and full labels.
#' @param which.traits integer identifying the traits to be selected.
#' @param column.name column names in \code{tab} as a variable in \code{lab}.
#' @param traitID trait identifier variable in \code{lab}.
#' @param modalityID modality identifier variable in \code{lab}.
#' @param delete.null.col if \code{TRUE}, removes null trait modality columns in
#' the resulting cross table and accordingly adjusts the labels.
#' @param delete.block if \code{TRUE}, removes traits having only one modality.
#' @param ori.col.name if \code{TRUE}, original column names of \code{tab} and in \code{lab$Column.name} are kept.
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
#' w <- fuz.tab(which.traits = c(3:7, 25, 28:29, 40))
#' w$lab
#' ### Select the traits "Sea floor affinity", "Motility" and "Feeding type"
#' w <- fuz.sel(w$tab, w$lab, which.traits = c(2, 6, 9))
#' w$lab
#'
#' @export

fuz.sel <- function(tab, lab, which.traits, column.name = "Column.name",
                    traitID = "Trait.ID", modalityID = "Modality.ID",
                    delete.null.col = TRUE, delete.block = TRUE,
                    ori.col.name = FALSE){
  tab <- tab[lab[,traitID] %in% which.traits]
  lab <- lab[lab[,traitID] %in% which.traits,]
  if(delete.null.col){
    w <- apply(tab, 2, sum)
    tab <- tab[w > 0]
    lab <- lab[lab[,column.name] %in% colnames(tab),]
  }
  if(delete.block){
    w <- which(table(lab[,traitID]) < 2)
    tab <- tab[lab[,traitID] %in% w == FALSE]
    lab <- lab[lab[,traitID] %in% w == FALSE,]
  }
  if(!ori.col.name){
    w <- table(lab[,traitID])
    lab[,traitID] <- rep(1:length(w), w)
    w <- as.numeric()
    for(i in 1:max(lab[,traitID])){
      w <- c(w, 1:length(lab[,traitID][lab[,traitID] == i]))
    }
    lab[,modalityID] <- w
    lab[,column.name] <- paste("T", lab[,traitID], ".M",
                             lab[,modalityID], sep = "")
    colnames(tab) <- lab[,column.name]
  }
  list(tab = tab, lab = lab)
}
