#' Extraction of bibliographical sources in MacroTraits data
#'
#' Returns the references of specified combinations of species and traits
#'
#' @param data long-format trait data frame containing referenced records. If missing, \code{MacroTraits$traits} as default.
#' @param lab lab data frame containing trait and modality identifiers and full labels. If missing, \code{MacroTraits$labels} as default.
#' @param ref data frame containing reference identifiers and full references. If missing, \code{MacroTraits$references} as default.
#' @param species vector of selected species to be documented.
#' @param which.traits vector of trait identifiers. All traits by default.
#' @param ref.type if \code{TRUE}, the type of reference (book, article, thesis...) is added.
#'
#' @return Returns a list of two data frames:
#' \describe{
#'   \item{\code{tab}}{A data frame containing trait data}
#'   \item{\code{lab}}{A data frame containing labels}
#' }
#'
#' @examples
#' data(MacroTraits)
#' ### Visualise the traits and their respective identifiers
#' unique(MacroTraits$labels[c("Trait.ID", "Trait")])
#' ### Vector of selected species
#' sp <- c("Corophium volutator", "Lanice conchilega", "Macoma balthica")
#' ### Query
#' w <- get.ref(sp = sp, which.traits = c(8, 9, 25, 40))
#' \dontrun{
#' View(w)
#' }
#'
#' @export

get.ref <- function(data, lab, ref, species, which.traits, ref.type = FALSE){
  if(missing(data)){
    data <- fuzR::MacroTraits$traits
  }
  if(missing(lab)){
    lab <- fuzR::MacroTraits$labels
  }
  if(missing(ref)){
    ref <- fuzR::MacroTraits$references
  }
  if(missing(which.traits)){
    which.traits <- 1:max(data$Trait.ID)
  }
  df <- data[c("Species", "Trait.ID", "Reference.ID")][data$Species %in% species,]
  df <- unique(df[df$Trait.ID %in% which.traits,])
  df <- unique(merge(df, lab[c("Trait.ID", "Trait")], by = "Trait.ID"))
  w <- numeric()
  id <- df$Reference.ID
  for(i in 1:nrow(df)){
    w[i] <- length(strsplit(x = df$Reference.ID[i], split = ",")[[1]])
  }
  df <- df[rep(1:nrow(df), w),]
  df$Reference.ID <- unlist(strsplit(x = id, split = ","))
  df <- merge(df, ref, by = "Reference.ID")
  df <- df[c("Species", "Trait.ID", "Trait", "Reference", "Reference.type")]
  df <- df[order(df$Species, df$Trait.ID, df$Reference),]
  rownames(df) <- 1:nrow(df)
  if(!ref.type){
    return(df[c("Species", "Trait", "Reference")])
  }else{
    return(df[c("Species", "Trait", "Reference.type", "Reference")])
  }
}
