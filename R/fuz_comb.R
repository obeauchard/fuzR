#' Creation of synthetic trait
#'
#' This function enables easy combination of different fuzzy traits into a formula.
#' Especially useful for trait-based indicator development.
#'
#' @param tab data frame containing fuzzy-coded traits.
#' @param lab data frame containing trait and modality identifiers and full labels.
#' @param which.traits integer identifying the traits to be included into a formula.
#' @param traitID trait identifier variable in \code{lab}.
#' @param list.sco list containing vectors of modality performance scores (one per trait).
#' @param formula character string expressing the arithmetic trait combination. Uses \code{x1},..., \code{xn}
#' that correspond respectively to identifiers in \code{which.traits} for n traits. Cumulative product as default.
#' @param fun function combining the scored values within a trait. See details.
#' @param fuz logical vector. Which traits should be transformed into frequencies within species profiles?
#' @param sco.scale if \code{TRUE}, the modality performance scores are rescaled between 0 and 1
#' @param trait.scale if \code{TRUE}, each trait is rescaled between 0 and 1
#' @param index.scale if \code{TRUE}, the final synthetic trait is rescaled between 0 and 1
#'
#' @details
#' Each trait is a block of trait modality columns, and each modality is attributed
#' a performance score contained in a respective vector in \code{list.sco}. Then, a unique value per trait
#' is calculated before being considered in the formula.
#'
#' Let's take the example of a fuzzy profile 0/1/1 and an associated vector of performance scores 1/2/3. If \code{fuz = TRUE},
#' the profile becomes 0.0/0.5/0.5. If \code{sco.scale = TRUE}, the performance scores become 0.33/0.67/1.00.
#' By muliplying both, the species performance profile is 0.000/0.335/0.500. A unique value can be represented
#' by the weighted mean, i.e., by the total sum = 0.835 indicated by \code{fun}.
#'
#' If \code{trait.scale = TRUE}, all traits are rescaled between 0 and 1 before being considered in the formula,
#' which gives them an equal weight. Finally, if \code{index.scale = TRUE}, the final synthetic trait
#' is rescaled between 0 and 1.
#'
#' @return Returns a synthetic trait as a vector of continuous values
#'
#' @examples
#' ### Example with biodeposition
#' ### as the product of body mass and suspension feeding
#' data(MacroTraits)
#' unique(MacroTraits$labels[c("Trait.ID", "Trait")])
#' w <- fuz.tab(which.traits = c(23, 40))
#' w$lab
#' ### Define performance scores for each trait
#' list.sco <- list(c(0.0005, 0.005, 0.05, 0.5, 5, 15),
#'                  c(0, 1, 0, 0, 0, 0))
#' tr <- fuz.comb(tab = w$tab, lab = w$lab,
#'                  which.traits = 1:2, list.sco = list.sco)
#' ### tr clearly exhibits outliers
#' hist(tr)
#' ### Convert tr into a fuzzy trait
#' hist(tr[tr > 0])
#' tr.fuz <- fuz.conv(tr, scan = FALSE,
#'                    interv.bound = c(0, 0.025, 0.1),
#'                    row.names = rownames(w$tab),
#'                    col.names = c("Null", "Low", "Intermediate", "High"))
#' head(tr.fuz)
#' apply(tr.fuz, 2, sum)
#'
#' @export

fuz.comb <- function(tab, lab, which.traits = NULL, traitID = "Trait.ID", list.sco,
                     formula = paste("x", 1:length(which.traits),
                                     sep = "", collapse = " * "),
                     fun = rep("sum", length(which.traits)),
                     fuz = rep(TRUE, length(which.traits)),
                     sco.scale = TRUE, trait.scale = TRUE, index.scale = FALSE){
  if(is.null(which.traits))
    which.traits <- 1:length(unique(lab[,traitID]))
  w1 <- sum(unlist(lapply(list.sco, length)))
  w2 <- ncol(tab[lab[,traitID] %in% which.traits])
  if(w1 != w2)
    stop("number of modalities not equal to number of scores")
  df <- data.frame(row.names = rownames(tab))
  for(i in 1:length(which.traits)){
    blo <- tab[lab[,traitID] == which.traits[i]]
    if(fuz[i])
      blo <- sweep(blo, 1, apply(blo, 1, sum), "/")
    blo[blo == "NaN"] <- 0
    sco <- list.sco[[i]]
    if(sco.scale)
      sco <- sco / max(sco)
    blo <- sweep(blo, 2, sco, "*")
    w <- numeric()
    for(j in 1:nrow(tab)){
      f <- match.fun(fun[i])
      if(sum(blo[j,]) > 0){
        w[j] <- f(blo[j,][blo[j,] > 0])
      }else{
        w[j] <- 0
      }
    }
    if(trait.scale)
      w <- w / max(w)
    df[,i] <- w
    colnames(df)[i] <- paste("x", i, sep = "")
  }
  w <- with(df, eval(parse(text = formula)))
  names(w) <- rownames(df)
  if(index.scale)
    w <- w / max(w)
  return(w)
}
