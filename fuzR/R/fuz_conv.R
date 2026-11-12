#' Convert a continuous trait into a fuzzy one
#'
#' \code{fuz.conv} generates a series of binary variables from a continuous trait. The user is free
#' to define interval boundaries. Useful for technical purposes such as homogenising a data set,
#' coping with outliers or circumventing problems of multivariate non-linearity.
#'
#' @param x a continuous trait.
#' @param scan if \code{TRUE}, plot \code{hist(x)} and ask for boundary choice.
#' @param include.0 if TRUE, plot \code{hist(x)}, otherwise \code{hist(x[x > 0])}.
#' @param interv.bound interval boundaries within \code{x}. See details.
#' @param col.names vector of modality column names for the resulting fuzzy-coded trait.
#' @param row.names vector of taxon names.
#'
#' @details
#' The function uses \code{cut(x, right = TRUE,...)} in which intervals are closed on the right.
#' Hence, if 0 should be considered as a separate category, use \code{interv.bound = c(0,...)}.
#' Press escape to leave the procedure.
#'
#' @return Returns a complete disjunctive table, i.e., a block of dummy variables.
#'
#' @examples
#' x <- sample(0:10, 100, replace = TRUE)^2
#' ### Choose yourself
#' \dontrun{
#' w <- fuz.conv(x)
#' w
#' }
#' ### Predefined boundaries
#' w <- fuz.conv(x, scan = FALSE, interv.bound = c(0, 20, 40))
#' w
#'
#' @export

fuz.conv <- function(x, scan = TRUE,
                     interv.bound = NULL, include.0 = FALSE,
                     col.names = NULL, row.names = NULL){
  if(scan & !is.null(interv.bound))
    interv.bound <- NULL
  if(scan){
    if(include.0){
      graphics::hist(x = x)
    }else{
      graphics::hist(x = x[x > 0])
    }
    cat("Choose a number of inner interval boundaries", "\n")
    nib <- readLines(n = 1)
    ib <- numeric()
    for(i in 1:nib){
      cat(paste("Choose boundary", i, sep = " "), "\n")
      ib[i] <- readLines(n = 1)
    }
    ib <- c(min(x)-1, ib, max(x))
  }else{
    if(is.null(interv.bound))
      stop("interv.bound is missing")
    ib <- c(min(x)-1, interv.bound, max(x))
  }
  w <- cut(x, breaks = ib)
  w <- stats::model.matrix(data = data.frame(w), ~ 0 + w)
  w <- data.frame(w)
  if(is.null(col.names)){
    colnames(w) <- paste("Modality", 1:ncol(w))
  }else{
    colnames(w) <- col.names
  }
  if(!is.null(row.names))
    rownames(w) <- row.names
  return(w)
}
