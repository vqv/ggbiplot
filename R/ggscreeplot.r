#' Screeplot for Principal Components
#' 
#' Produces scree plots (Cattell, 1966) of the variance proportions explained by each dimension against dimension number from 
#' various dimension reduction techniques
#'
#' @param pcobj   an object representing a linear dimension technique, such a returned by \code{\link[stats]{prcomp}} 
#'                or \code{\link[stats]{princomp}} or \code{\link[FactoMineR]{PCA}} or \code{\link[MASS]{lda}}
#' @param type    the type of scree plot.  
#'                'pev' corresponds proportion of explained variance, i.e. the eigenvalues divided by the trace. 
#'                'cev' corresponds to the cumulative proportion of explained variance, i.e. the partial sum of the first k eigenvalues divided by the trace.
#' @param size    point size
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
                        size = 4) 
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
  } else {
    stop('Expected a object of class "prcomp", "princomp", "PCA", or "lda"')
  }
  

  type <- match.arg(type)
  d <- pcobj$sdev^2
  yvar <- switch(type, 
                 pev = d / sum(d), 
                 cev = cumsum(d) / sum(d))

  yvar.lab <- switch(type,
                     pev = 'Proportion of explained variance',
                     cev = 'Cumulative proportion of explained variance')

  PC <- NULL
  df <- data.frame(PC = 1:length(d), yvar = yvar)

  ggplot(data = df, aes(x = PC, y = yvar)) + 
    xlab('Principal component number') + 
    ylab(yvar.lab) +
    geom_point(size = size) + geom_path()
}
