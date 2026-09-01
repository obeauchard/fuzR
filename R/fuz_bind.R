#' Binds two or more fuzzy-coded trait data tables
#'
#' Merge two or more fuzzy-coded trait data sets and create common trait and modality labels.
#' Useful when analysing separately different trait data sets next to a global analysis.
#'
#' @param list.tab a list of data frames with identical row names, each containing at least one trait.
#' @param list.lab a list of trait label data frames that respectively correspond to the data frames in \code{list.tab}.
#' @param column.name column names in \code{list.tab} as a variable in \code{list.lab}.
#' @param traitID trait identifier in \code{list.lab}.
#' @param trait full trait labels in \code{list.lab}.
#' @param modality full modality labels in \code{list.lab}.
#'
#' @return Returns a list of two data frames:
#' \describe{
#'   \item{\code{tab}}{A data frame containing trait data}
#'   \item{\code{lab}}{A data frame containing labels}
#' }
#'
#' @examples
#' ### Building a data frame of traits expressing ecosystem functions
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
#'
#' ### Sediment biodiffusive mixing
#' w <- fuz.tab(which.traits = c(4, 6, 23, 26, 37))
#' w$lab
#' list.sco <- list(1:5,                                # Sea floor affinity
#'                  c(1:5, 0),                          # Substratum depth occupancy
#'                  c(0.0005, 0.005, 0.05, 0.5, 5, 15), # Body mass
#'                  0:4,                                # Mobility
#'                  c(0, 1, 0, 0, 0))                   # Sediment biomixing type
#' biodif <- fuz.comb(tab = w$tab, lab = w$lab,
#'                    which.traits = 1:5, list.sco = list.sco)
#' hist(biodif[biodif > 0])
#' biodif.fuz <- fuz.conv(biodif, scan = FALSE,
#'                        interv.bound = c(0, 0.02, 0.04, 0.06),
#'                        row.names = names(biodif))
#' ### Create labels
#' biodif.fuz <- fuz.lab(tab = biodif.fuz, blo = 5,
#'                       traits = "Biodiffusion",
#'                       modalities = c("Null", "Low", "Intermediate", "High", "Very high"))
#'
#' ### bind both traits
#' w <- fuz.bind(list(biodep.fuz$tab, biodif.fuz$tab),
#'               list(biodep.fuz$lab, biodif.fuz$lab))
#' head(w$tab)
#' w$lab
#'
#' @export

fuz.bind <- function(list.tab, list.lab,
                     column.name = "Column.name", traitID = "Trait.ID",
                     trait = "Trait", modality = "Modality"){
  Trait.ID <- numeric()
  for(i in 1:length(list.lab)){
    if(!is.data.frame(list.lab[[i]])){
      stop("data.frame expected")
    }else{
      Trait.ID <- c(Trait.ID, table(list.lab[[i]][,traitID]))
    }
  }
  Trait.ID <- rep(1:length(Trait.ID), Trait.ID)
  Modality.ID <- numeric()
  for(i in 1:max(Trait.ID)){
    Modality.ID <- c(Modality.ID, 1:length(Trait.ID[Trait.ID == i]))
  }
  Column.name <- paste("T", Trait.ID, ".M", Modality.ID, sep = "")
  Trait <- as.character()
  Modality <- as.character()
  for(i in 1:length(list.lab)){
    Trait <- c(Trait, list.lab[[i]][,trait])
    Modality <- c(Modality, list.lab[[i]][,modality])
  }
  lab <- data.frame(Column.name, Trait.ID,
                    Modality.ID, Trait, Modality)
  indic <- as.numeric()
  for(i in 1:length(list.tab)){
    if(!is.data.frame(list.tab[[i]])){
      stop("data.frame expected")
    }else{
      indic <- c(indic, rep(i, ncol(list.tab[[i]])))
    }
  }
  tab <- list.tab[[1]]
  colnames(tab) <- Column.name[indic == 1]
  for(i in 2:length(list.tab)){
    w <- list.tab[[i]]
    colnames(w) <- Column.name[indic == i]
    if(!identical(rownames(tab), rownames(w))){
      stop("same row names expected")
    }else{
      tab <- cbind(tab, w)
    }
  }
  list(tab = tab, lab = lab)
}
