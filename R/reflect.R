
#' Reflect columns in a principal component-like object
#' 
#' Principle component like objects have variable loadings whose
#' signs are arbitrary, in the sense that a given column can be 
#' reflected (multiplied by -1) without changing the fit.
#' 
#' This function allows one to reflect any columns of the variable
#' loadings (and corresponding observation scores). This is often
#' useful for interpreting a biplot.
#'
#' @param pcobj     an object returned by \code{\link[stats]{prcomp}}, \code{\link[stats]{princomp}}, 
#'                  \code{\link[FactoMineR]{PCA}}, or \code{\link[MASS]{lda}}
#' @param columns   a vector of indices of the columns to reflect
#'
#' @return    The pca-like object with specified columns of the 
#'            variable loadings and observation scores multiplied
#'            by -1.
#' @export
#'
#' @examples
#' # none yet
reflect <- function(pcobj, columns = 1:2){

  check <- function(x, cols){
    if(!all(cols %in% 1:ncol(x))) stop("Illegal columns selected:",
                                       paste(cols, collapse = ", "))
  }

  if(inherits(pcobj, 'prcomp')){
    check(pcobj$rotation, columns)
    pcobj$rotation[, columns] <- -1 * pcobj$rotation[, columns]
    pcobj$x[, columns]        <- -1 * pcobj$x[, columns]
  } 
  else if(inherits(pcobj, 'princomp')) {
    check(pcobj$loadings, columns)
    pcobj$loadings[, columns] <- -1 * pcobj$loadings[, columns]
    pcobj$scores[, columns]   <- -1 * pcobj$scores[, columns]
  } 
  else if(inherits(pcobj, 'PCA')) {
    check(pcobj$var$coord, columns)
    pcobj$var$coord[, columns] <- -1 * pcobj$var$coord[, columns]
    pcobj$ind$coord[, columns]   <- -1 * pcobj$ind$coord[, columns]
  } 
  else if(inherits(pcobj, "lda")) {
    warning("Can't reflect an 'lda' object")
  #   u <- predict(pcobj)$x
  #   check(u, columns)
  #   pcobj$scaling[, columns] <- -1 * pcobj$scaling[, columns]
  #   pcobj$x[, columns]       <- -1 * pcobj$x[, columns]
  }
  else {
    stop('Expected a object of class "prcomp", "princomp", "PCA", or "lda"')
  }
  
  pcobj
  
}