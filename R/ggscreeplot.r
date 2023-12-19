#' Screeplot for Principal Components
#' 
#' Produces scree plots (Cattell, 1966) of the variance proportions explained by each dimension against dimension number from 
#' various dimension reduction techniques
#'
#' @param pcobj   an object returned by \code{\link[stats]{prcomp}}, \code{\link[stats]{princomp}}, 
#'                \code{\link[FactoMineR]{PCA}}, \code{\link[ade4]{dudi.pca}}, or \code{\link[MASS]{lda}}
#' @param type    the type of scree plot.  
#'                'pev' corresponds proportion of explained variance, i.e. the eigenvalues divided by the trace. 
#'                'cev' corresponds to the cumulative proportion of explained variance, i.e. the partial sum of the first k eigenvalues divided by the trace.
#' @param size    point size
#' @param shape   shape of the points
#' @param color   color for points and line
#' @param linetype type of line
#' @param linewidth width of line
#' 
#' @returns A ggplot2 object
#' @export
#' @references 
#' Cattell, R. B. (1966). The Scree Test For The Number Of Factors. \emph{Multivariate Behavioral Research}, 1, 245–276.
#' @examples
#'   data(wine)
#'   wine.pca <- prcomp(wine, scale. = TRUE)
#'   ggscreeplot(wine.pca)
#'
ggscreeplot <- function(pcobj, 
                        type = c('pev', 'cev'),
                        size = 4,
                        shape = 1,
                        color = "black",
                        linetype = 1,
                        linewidth = 1) 
{
  # Recover the SVD
  if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
  } else if(inherits(pcobj, "lda")) {
    d <- pcobj$svd
  } else if(inherits(pcobj, 'pca') & inherits(pcobj, 'dudi')){
    d <- sqrt(pcobj$eig)
  }
  else {
    stop('Expected a object of class "prcomp", "princomp", "PCA", c("pca", "dudi") or "lda"')
  }
  

  type <- match.arg(type)
  dsq <- d^2
  yvar <- switch(type, 
                 pev = dsq / sum(dsq), 
                 cev = cumsum(dsq) / sum(dsq))

  yvar.lab <- switch(type,
                     pev = 'Proportion of explained variance',
                     cev = 'Cumulative proportion of explained variance')

  PC <- NULL
  df <- data.frame(PC = 1:length(d), yvar = yvar)

  ggplot(data = df, aes(x = PC, y = yvar)) + 
    xlab('Principal component number') + 
    ylab(yvar.lab) +
    geom_point(size = size, shape = shape, color = color) + 
    geom_path(linetype = linetype, linewidth = linewidth, color = color)
}
