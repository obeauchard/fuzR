#' Averaging trait data at different taxonomic levels and filling missing biological information
#'
#' As biological trait data are generally defined at the species level, trait data are
#' often missing in field inventories in which taxa are not always identified at this level.
#' Besides, sampled species can be absent from trait documentations whereas congeneric homologues
#' can be documented and used to fill missing information.
#'
#' @param txn taxonomic tree as a taxa by taxonomic levels data frame, starting from higher levels
#' on the right (e.g., phylum) to lower levels toward the left (e.g., species).
#' @param tab data frame containing fuzzy traits.
#' @param lab data frame containing trait and modality identifiers and full labels.
#' @param txc taxocenosis, i.e., taxa identified at various levels as a vector.
#' @param max.lev column identifier of \code{txn}. It is the highest taxonomic level to which
#' trait data in \code{txc} can be averaged. Must be element of \code{1:ncol(txn)}.
#' @param genus column name of the genus level in \code{txn}.
#' @param restrict.clade list of vectors containing taxonomic names. For instance, when averaging
#' trait data for a given genus, if \code{txn} contains 10 species of this genus and only 2 are
#' biogeographically relevant (i.e., same province), only those 2 species will be considered.
#' @param round.sco if not \code{NULL}, value between 0 and 1 below and above which
#' the averaged score can be rounded to respectively 0 and 1.
#'
#' @return Returns a list of four objects:
#' \describe{
#'   \item{\code{tab}}{A data frame containing trait data}
#'   \item{\code{lab}}{A data frame containing trait and modality labels}
#'   \item{\code{tax}}{A data frame containing the taxonomy of \code{txc}}
#'   \item{\code{not.found}}{A vector containing taxa from \code{txc} that could not be documented}
#' }
#'
#' @examples
#' data(MacroTraits)
#' w <- fuz.tab(which.traits = 8)
#' tab <- w$tab
#' lab <- w$lab
#' ### Ophiocten sericeum not documented
#' ### while other species from Ophiocten are documented
#' txc <- c("Amphiura filiformis", "Corophium",
#'          "Mysidae", "Isopoda", "Ophiocten sericeum",
#'          "Unknown")
#' w <- fuz.avg(txn = MacroTraits$taxonomy[8:13],
#'              tab = tab, lab = lab, txc = txc)
#' w$tab
#' w$tax
#' w$not.found
#'
#' @export

fuz.avg <- function(txn, tab, lab, txc, max.lev = 1,
                    genus = "Genus", restrict.clade = NULL,
                    round.sco = NULL){
  if(!max.lev %in% 1:ncol(txn))
    stop("max.lev must be within 1:ncol(txn)")
  #Averaged traits at all levels
  tab.avg <- tab
  for(i in max.lev:(ncol(txn)-1)){
    w <- apply(tab, 2, function(x) tapply(x, factor(txn[,i]), base::mean))
    w <- data.frame(w)
    tab.avg <- rbind(tab.avg, w)
  }
  #Restrict clades according to specifications
  if(!is.null(restrict.clade)){
    for(i in 1:length(restrict.clade)){
      w <- sapply(txn, function(x) restrict.clade[[i]] %in% x)
      w <- matrix(w, ncol = ncol(txn))
      lev <- colnames(txn)[apply(w, 2, function(x) TRUE %in% x)]
      lev <- which(colnames(txn) == lev)
      w <- txn[txn[,lev] %in% restrict.clade[[i]],]
      w <- unique(w[,lev-1])
      w1 <- txn[!txn[,lev-1] %in% w,]
      w2 <- txn[txn[,lev] %in% restrict.clade[[i]],]
      txn <- rbind(w1, w2)
    }
  }
  #Taxa not found
  not.found <- txc[!txc %in% rownames(tab.avg)]
  #Split possible taxa not found for which the genus can be documented
  w <- do.call(what = rbind, args = strsplit(not.found, " "))[,1]
  df.not.found <- data.frame(not.found = not.found, genus = w)
  df.not.found <- df.not.found[df.not.found$genus %in% rownames(tab.avg),]
  not.found <- not.found[!not.found %in% df.not.found$not.found]
  #Retrieve the corresponding taxonomy
  if(genus %in% colnames(txn)){
    gen.id <- which(colnames(txn) == genus)
    tab.tax.not.found <- unique(txn[1:gen.id][txn[,gen.id] %in% df.not.found$genus, drop = FALSE,])
    tab.tax.not.found <- tab.tax.not.found[order(tab.tax.not.found[,genus]), drop = FALSE,]
    w <- rep(1:nrow(tab.tax.not.found), table(df.not.found$genus))
    tab.tax.not.found <- tab.tax.not.found[w, drop = FALSE,]
    tab.tax.not.found$taxon <- df.not.found$not.found
    tab.tax.not.found$level <- 1
  }
  #Extract trait data
  w1 <- tab.avg[rownames(tab.avg) %in% txc, drop = FALSE,]
  w2 <- tab.avg[df.not.found$genus, drop = FALSE,]
  rownames(w2) <- df.not.found$not.found
  tab.res <- rbind(w1, w2)
  tab.res <- tab.res[order(rownames(tab.res)),]
  #Extract the taxonomy of the documented taxa
  tab.tax <- data.frame()
  for(i in ncol(txn):1){
    w <- txn[txn[,i] %in% rownames(w1),]
    w$level <- rep((ncol(txn)+1) - i, nrow(w))
    if(i < ncol(txn) & length(w[,i]) > 0)
      w[(i+1):ncol(txn)] <- ""
    w$Species <- w[,i]
    tab.tax <- unique(rbind(tab.tax, w))
  }
  colnames(tab.tax)[ncol(tab.tax)-1] <- "taxon"
  tab.tax <- rbind(tab.tax, tab.tax.not.found)
  tab.tax <- tab.tax[order(tab.tax$taxon),]
  if(!is.null(round.sco)){
    tab.res[tab.res < round.sco] <- 0
    tab.res[tab.res > round.sco] <- 1
  }
  tab.res <- tab.res[apply(tab.res, 2, sum) > 0]
  lab <- lab[lab$Column.name %in% colnames(tab.res),]
  w <- fuz.lab(tab = tab.res, blo = table(lab$Trait.ID),
               traits = unique(lab$Trait), modalities = lab$Modality)
  list(tab = w$tab, lab = w$lab,
       tax = tab.tax, not.found = not.found)
}
