#' Plotting taxa and modlities of a fuzzy trait along a continuous variable
#'
#' This function enables the graphical representation of a trait through its different modalities
#' along a continuous variable such as a multivariate axis or any other continuous ecological descriptor
#' along which tha taxa documented for the trait can be positioned.
#'
#' @param x data frame either as a fuzzy trait or containing factors.
#' @param y continuous variable as a vector.
#' @param mct measure of central tendency, either \code{median} or \code{mean}.
#' @param bar error bar representing trait modality variation. \code{"pc"} for percentile,
#' \code{"sd"} for standard deviation and \code{"ci"} for confidence interval.
#' @param pc if \code{bar = "pc"}, vector of lower and upper percentiles, respectively.
#' @param horiz if \code{TRUE}, modality distributions are represented horizontally.
#' @param rev.mod if \code{TRUE}, reverse the order of modality representation from top to bottom.
#' @param yax if \code{FALSE}, no y-axis is displayed.
#' @param ylim range of variable y.
#' @param ylab y-axis label.
#' @param mod modality labels.
#' @param cex.mod character size of modality labels.
#' @param pos.mod margin line for positioning modality labels.
#' @param pch.pt taxon plotting symbol type.
#' @param col.pt taxon plotting symbol colour.
#' @param bg.pt taxon plotting symbol background colour.
#' @param cex.pt taxon plotting symbol size.
#' @param pch.mct \code{mct} plotting symbol type.
#' @param col.mct \code{mct} plotting symbol colour.
#' @param bg.mct \code{mct} plotting symbol background colour.
#' @param cex.mct \code{mct} plotting symbol size.
#' @param jitter if \code{TRUE}, add noise orthogonally to y within modality distributions.
#' @param amount degree of noise if \code{jitter} is \code{TRUE}.
#' @param bar.lwd error bar width.
#' @param bar.col error bar colour.
#' @param return if \code{TRUE}, modality or factor level coordinates are returned.
#'
#' @examples
#' ### Illustration of trade-off in reproductive strategy
#' data(MacroTraits)
#' unique(MacroTraits$labels[c("Trait.ID", "Trait")])
#' ### Species by reproductive traits cross table
#' w <- fuz.tab(which.traits = c(10, 14:18))
#' tab <- w$tab
#' lab <- w$lab
#' indic <- lab$Trait.ID
#' ### Fuzzy Correspondence Analysis (FCA) with ade4
#' fuz <- ade4::prep.fuzzy.var(df = tab, col.blocks = table(indic))
#' fca <- ade4::dudi.fca(fuz, scannf = FALSE)
#' par(mfrow = c(3, 2), mar = c(4, 8, 3, 1))
#' ### Representation of traits along the first FCA axis
#' for(i in 1:max(indic)){
#'   # Order trait modalities according to their respective first axis score
#'   x <- tab[indic == i][order(fca$co[,1][indic == i])]
#'   # Similarly for modality labels
#'   mod <- lab$Modality[indic == i][order(fca$co[,1][indic == i])]
#'   # Plotting execution
#'   fuz.plot(x = x, y = fca$li[,1], ylab = "", mod = mod)
#'   # Add trait label
#'   mtext(unique(lab$Trait)[i], cex = 1, adj = 0, line = 1)
#' }
#'
#' ### When x contains factors
#' x <- data.frame(x1 = gl(2, 60), x2 = gl(2, 30, 120), x3 = gl(3, 10, 120))
#' y <- runif(nrow(x))
#' fuz.plot(x, y)

#' @export

fuz.plot <- function(x, y, mct = stats::median, bar = "pc",
                     pc = c(0.25, 0.75), horiz = TRUE, rev.mod = TRUE,
                     yax = TRUE, ylim = NULL, ylab = "y",
                     mod = NULL, cex.mod = 1, pos.mod = 1,
                     pch.pt = 1, col.pt = "grey", bg.pt = NULL, cex.pt = 1,
                     pch.mct = 20, col.mct = 1, bg.mct = NULL, cex.mct = 2,
                     jitter = TRUE, amount = 0.15, bar.lwd = 1, bar.col = 1,
                     return = FALSE){
  if(!is.data.frame(x))
    stop("x must be a data frame")
  if(!is.factor(x[,1])){
    w <- apply(x, 2, sum)
    x <- x[w > 0]
    if(is.null(mod)){
      mod <- colnames(x)
    }else{
      mod <- mod[w > 0]
    }
    y <- y[!apply(is.na(x), 1, sum) > 0]
    x <- stats::na.omit(x)
    y <- rep(y, ncol(x))
    y <- y[x > 0]
    w <- as.numeric(gl(ncol(x), nrow(x)))
    x <- matrix(as.matrix(x))[,1]
    x <- w[x > 0]
    y.mct <- tapply(y, factor(x), mct)
  }else{
    y <- y[do.call(order, x)]
    x <- x[do.call(order, x), drop = FALSE,]
    if(is.null(mod))
      mod <- as.character(interaction(unique(x), drop = TRUE, sep = ":"))
    for(i in 1:ncol(x)){
      x[,i] <- as.numeric(x[,i])
      w <- c(0, diff(x[,i]))
      w[w != 0] <- 1
      x[,i] <- cumsum(w)
    }
    x <- apply(x, 1, sum) + 1
    y.mct <- tapply(y, factor(x), mct)
  }
  x.mct <- unique(x)
  if(!horiz){
    rev.mod <- FALSE
  }else if(rev.mod){
    x <- -x
    x.mct <- -x.mct
  }
  bar.val <- matrix(0, length(mod), 2)
  for(i in 1:length(mod)){
    w <- y[abs(x) == unique(abs(x))[i]]
    if(bar == "sd"){
      w1 <- y.mct[i] - stats::sd(w)
      w2 <- y.mct[i] + stats::sd(w)
    }else if(bar == "ci"){
      w1 <- y.mct[i] - 1.96 * stats::sd(w) / length(w)^0.5
      w2 <- y.mct[i] + 1.96 * stats::sd(w) / length(w)^0.5
    }else if(bar == "pc"){
      w1 <- stats::quantile(w, pc[1])
      w2 <- stats::quantile(w, pc[2])
    }
    bar.val[i,] <- c(w1, w2)
  }
  x.coord <- x
  y.coord <- y
  x.coord.mct <- x.mct
  y.coord.mct <- y.mct
  x.coord.bar1 <- x.mct
  x.coord.bar2 <- x.mct
  y.coord.bar1 <- bar.val[,1]
  y.coord.bar2 <- bar.val[,2]
  if(jitter)
    x.coord <- base::jitter(x.coord, amount = amount)
  mod.side <- 1
  y.side <- 2
  xlim <- c(min(x)-0.5, max(x)+0.5)
  if(is.null(ylim))
    ylim <- range(base::pretty(y))
  xlab <- ""
  ylab <- ylab
  if(horiz){
    x.coord <- y
    y.coord <- x
    if(jitter)
      y.coord <- base::jitter(y.coord, amount = amount)
    x.coord.mct <- y.mct
    y.coord.mct <- x.mct
    x.coord.bar1 <- bar.val[,1]
    x.coord.bar2 <- bar.val[,2]
    y.coord.bar1 <- x.mct
    y.coord.bar2 <- x.mct
    mod.side <- 2
    y.side <- 1
    xlim <- ylim
    ylim <- c(min(y.coord)-0.5, max(y.coord)+0.5)
    xlab <- ylab
    ylab <- ""
  }
  if(!horiz){
    w <- y.coord
  }else{
    w <- x.coord
  }
  if(length(col.pt) > 1) 
    col.pt <- col.pt[cut(w, breaks = length(col.pt), labels = FALSE)]
  if(length(bg.pt) > 1) 
    bg.pt <- bg.pt[cut(w, breaks = length(bg.pt), labels = FALSE)]
  graphics::plot(x.coord, y.coord, xaxt = "n", yaxt = "n", xlab = xlab, ylab = ylab, bty = "n",
                 pch = pch.pt, col = col.pt, bg = bg.pt, cex = cex.pt,
                 xlim = xlim, ylim = ylim)
  graphics::segments(x.coord.bar1, y.coord.bar1, x.coord.bar2, y.coord.bar2,
                     lwd = bar.lwd, col = bar.col)
  graphics::points(x.coord.mct, y.coord.mct, pch = pch.mct, col = col.mct,
                   bg = bg.mct, cex = cex.mct)
  graphics::par(mgp = c(3, pos.mod, 0))
  graphics::axis(side = mod.side, at = unique(x), labels = mod, cex.axis = cex.mod, las = 1)
  graphics::par(mgp = c(3, 1, 0))
  if(yax)
    graphics::axis(side = y.side)
  if(return)
    unique(x)
}
