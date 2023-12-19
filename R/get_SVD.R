#' Extract the SVD components from a PCA-like object
#'
#' @description
#' 
#' Biplots are based on the Singular Value Decomposition, which for a data matrix is
#' \deqn{\mathbf{X} / \sqrt{n} = \mathbf{U} \mathbf{D} \mathbf{V}^T}
#' but these are computed and returned in quite different
#' forms by various PCA-like methods. This function provides a common interface, returning
#' the components with standard names.
#'
#'
#' @param pcobj  an object returned by \code{\link[stats]{prcomp}}, \code{\link[stats]{princomp}}, 
#'               \code{\link[FactoMineR]{PCA}}, \code{\link[ade4]{dudi.pca}}, or \code{\link[MASS]{lda}}
#'
#' @return  A list of four elements
#'   \describe{
#'     \item{n}{The sample size on which the analysis was based}
#'     \item{U}{Left singular vectors, giving observation scores}
#'     \item{D}{vector of singular values, the diagonal elements of the matrix \eqn{\mathbf{D}}, which are also the square roots
#'         of the eigenvalues of \eqn{\mathbf{X} \mathbf{X}'}}
#'     \item{V}{Right singular vectors, giving variable loadings}
#'   }
#' @export
#'
#' @examples
#' data(crime)
#' crime.pca <- 
#'   crime |> 
#'   dplyr::select(where(is.numeric)) |>
#'   prcomp(scale. = TRUE)
#' 
#' crime.svd <- get_SVD(crime.pca)
#' names(crime.svd)
#  # singular values
#' crime.svd$D
#'
get_SVD <- function(pcobj) {

# Recover the SVD from a PCA-like object
  if(inherits(pcobj, 'prcomp')){
    n <- nrow(pcobj$x)
    D <- pcobj$sdev
    U <- sweep(pcobj$x, 2, 1 / (D * sqrt(n)), FUN = '*')
    V <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    n <- pcobj$n.obs
    D <- pcobj$sdev
    U <- sweep(pcobj$scores, 2, 1 / (D * sqrt(n)), FUN = '*')
    V <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    n <- nrow(pcobj$call$X)
    D <- unlist(sqrt(pcobj$eig)[1])
    U <- sweep(pcobj$ind$coord, 2, 1 / (D * sqrt(n)), FUN = '*')
    V <- sweep(pcobj$var$coord, 2, sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]), FUN="/")
  } else if(inherits(pcobj, "lda")) {
    n <- pcobj$N
    D <- pcobj$svd
    U <- predict(pcobj)$x/sqrt(n)
    V <- pcobj$scaling
  } else if(inherits(pcobj, 'pca') & inherits(pcobj, 'dudi')){
    n <- nrow(pcobj$tab)
    D <- sqrt(pcobj$eig)
    U <- pcobj$li
    V <- pcobj$co
  }
  else {
    stop('Expected a object of class "prcomp", "princomp", "PCA", c("pca", "dudi") or "lda"')
  }
  
  list(n=n, U=U, D=D, V=V)

}
