#' Creation of trait data cross table and associated labels
#'
#' From a long-format data table, returns a cross table of species by fuzzy-coded trait modalities
#' and associated trait and modality labels.
#'
#' @param data long-format trait data frame. If missing, \code{MacroTraits$traits} as default.
#' @param lab data frame containing trait and modality identifiers and full labels. If missing, \code{MacroTraits$labels} as default.
#' @param traitID column name of trait identifier in \code{data} and \code{lab}.
#' @param modalityID column name of modality identifier in \code{data} and \code{lab}.
#' @param species column name of modality identifier in \code{data}.
#' @param trait column name of full trait label in \code{lab}.
#' @param modality column name of full modality label in \code{lab}.
#' @param score column name of trait modality score in \code{data}.
#' @param which.traits vector of trait identifiers. All traits by default.
#' @param mis.dat if \code{TRUE}, indicates missing scores in \code{data}.
#' @param delete.null.col if \code{TRUE}, removes null trait modality columns
#' in the resulting cross table and accordingly adjusts the labels.
#' @param check.sp.records if \code{TRUE}, verifies whether all species
#' are evenly documented: are there species for which trait modalities are missing?
#'
#' @return Returns a list of two data frames:
#' \describe{
#'   \item{\code{tab}}{A data frame containing trait data}
#'   \item{\code{lab}}{A data frame containing labels}
#' }
#'
#' @examples
#' data(MacroTraits)
#'
#' ### Show traits and respective identifiers
#' unique(MacroTraits$labels[c("Trait.ID", "Trait")])
#' w <- fuz.tab(which.traits = c(3:7, 25, 28:29, 40))
#' head(w$tab[1:10])
#' w$lab[1:10,]
#'
#' ### Fuzzy Correspondence Analysis with ade4
#' indic <- w$lab$Trait.ID
#' fuz <- ade4::prep.fuzzy.var(df = w$tab, col.blocks = table(indic))
#' fca <- ade4::dudi.fca(fuz, scannf = FALSE)
#' par(mfrow = c(3, 3), mar = rep(0.1, 4))
#' for(i in 1:max(indic)){
#'   ade4::s.label(fca$li, cpoint = 0, clab = 0, cgrid = 1.5)
#'   points(fca$li, col = "grey")
#'   tr <- unique(w$lab$Trait)[i]
#'   mod <- w$lab$Modality[indic == i]
#'   ade4::s.distri(fca$li, fuz[indic == i], cstar = 0, cpoint = 0,
#'                  lab = mod, clab = 1.5, sub = tr, possub = "topleft",
#'                  csub = 2, add.p = TRUE)
#' }
#'
#' @export

fuz.tab <- function(data, lab, traitID = "Trait.ID", modalityID = "Modality.ID",
                    species = "Species", trait = "Trait", modality = "Modality",
                    score = "Score", which.traits = NULL, mis.dat = TRUE,
                    delete.null.col = TRUE, check.sp.records = TRUE){
  if(missing(data)){
    data <- fuzR::MacroTraits$traits
  }
  if(missing(lab)){
    lab <- fuzR::MacroTraits$labels
  }
  if(!is.data.frame(data))
    stop("data.frame expected")
  if(!is.data.frame(lab))
    stop("data.frame expected")
  df <- merge(data, lab)
  w <- which(colnames(df) %in% c(species, trait, modality))
  for(i in w){
    df[,i] <- as.character(df[,i])
  }
  if(check.sp.records){
    w <- df[c(species, trait, modality)]
    w <- table(w[,species], w[,trait])
    w <- apply(w, 2, function(x) unique(x))
    w <- lapply(w, function(x) length(x))
    w <- unlist(w)
    if(length(unique(w)) > 1){
      print(names(w)[w > 1])
      stop("Uneven species information or trait labels")
    }
  }
  w <- unique(df[c(traitID, trait)])
  w1 <- table(unique(w[c(traitID, trait)])[,trait])
  w2 <- table(unique(w[c(traitID, trait)])[,traitID])
  w <- apply(cbind(w1, w2), 1, function(x) length(unique(x)))
  if(length(unique(w)) > 1){
    print(names(w)[w > 1])
    stop("Uneven trait IDs or labels")
  }
  w <- unique(df[c(trait, modalityID, modality)])
  w1 <- table(unique(w[c(trait, modalityID)])[,trait])
  w2 <- table(unique(w[c(trait, modality)])[,trait])
  w3 <- table(w[,trait])
  w <- apply(cbind(w1, w2, w3), 1, function(x) length(unique(x)))
  if(length(unique(w)) > 1){
    print(names(w)[w > 1])
    stop("Uneven modality IDs or labels")
  }
  if(mis.dat){
    w <- stats::aggregate(df[,score] ~ df[,species] + df[,traitID], FUN = sum)
    colnames(w) <- c(species, traitID, score)
    w <- sort(unique(w[,species][w[,score] == 0]))
    if(length(w) > 0){
      print(w)
      stop("Missing data")
    }
  }
  if(!is.null(which.traits)){
    df <- df[df[,traitID] %in% which.traits,]
    df <- df[order(df[,traitID]),]
    w <- numeric()
    for(i in 1:length(which.traits)){
      w1 <- sort(which.traits)[i]
      w1 <- which(which.traits == w1)
      w <- c(w, rep(w1, table(df[,traitID])[i]))
    }
    df[,traitID] <- w
  }
  df <- df[order(df[,species], df[,traitID], df[,modalityID]),]
  w <- unique(df[c(traitID, modalityID, trait, modality)])
  w[,traitID] <- rep(1:length(unique(w[,trait])), table(w[,traitID]))
  tab <- matrix(df[,score], ncol = nrow(w), byrow = TRUE)
  rownames(tab) <- unique(df[,species])
  if(delete.null.col){
    sumcol <- apply(tab, 2, sum)
    if(any(sumcol == 0)){
      tab <- tab[,sumcol > 0]
      w <- w[sumcol > 0,]
      for(i in 1:max(w[,traitID])){
        w[,modalityID][w[,traitID] == i] <- 1:length(w[,traitID][w[,traitID] == i])
      }
    }
  }
  cn <- rep(as.character("T"), nrow(w))
  cn <- paste(cn, w[,traitID], sep = "")
  cn <- paste(cn, paste(rep("M", nrow(w)), w[,modalityID], sep = ""), sep = ".")
  colnames(tab) <- cn
  tab <- data.frame(tab, check.names = FALSE)
  lab <- cbind(colnames(tab), w)
  rownames(lab) <- 1: nrow(lab)
  colnames(lab)[1] <- "Column.name"
  list(tab = tab, lab = lab)
}
