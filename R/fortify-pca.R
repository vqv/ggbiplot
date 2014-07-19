#' Fortify methods for principal components analysis objects produced 
#' by \code{prcomp}, \code{princomp}, and \code{PCA} in \pkg{FactoMineR}
#' 
#' @name fortify-pca
#'
#' @param model   an object of class \code{prcomp}, \code{princomp}, or 
#'                \code{\link[FactoMineR]{PCA}}
#' @param data    principal component scores are computed for data; not used 
#'                for \code{\link[FactoMineR]{PCA}} objects 
#' @param scale   scale the scores and loadings as done by \code{\link[stats]{biplot.prcomp}}
#' @param equalize should the basis vectors be rescaled to be comparable with the score vectors?
#' @param ...     not used
#'
#' @return A data frame consisting of the columns of \code{data} together with 
#'         principal component scores.  Additionally, the attribute 
#'         \code{basis} is set to a data frame containing the loadings and a 
#'         column of variable names if present in \code{model}
#'
#' @examples
#' data(wine)
#' winepca <- prcomp(wine, scale = TRUE)
#' cultivar <- wine.class
#' df <- fortify(winepca, wine)
#' ggplot(df, aes(x = PC1, y = PC2)) +
#'   geom_point(aes(group = cultivar, color = cultivar)) + 
#'   stat_ellipse(aes(group = cultivar, color = cultivar)) +
#'   geom_axis(data = attr(df, "basis"), aes(label = .name), scale = 3) +
#'   coord_equal()
NULL

.fortify_pca <- function(scores, loadings, sdev, scale, equalize, data) {

  scores <- sweep(scores, 2, sdev^(-scale), FUN = "*")
  loadings <- sweep(loadings, 2, sdev^(scale), FUN = "*")
  if(equalize) {
    r <- sqrt( median(rowSums(scores^2)) / max(colSums(loadings^2)) )
    loadings <- loadings * r
  }
  scores <- data.frame(scores)
  loadings <- data.frame(loadings)
  names(scores) <- paste("PC", seq_len(ncol(scores)), sep = "")
  names(loadings) <- paste("PC", seq_len(ncol(loadings)), sep = "")

  if (!is.null(data)) scores <- cbind(data, scores)
  if (!is.null(rownames(loadings))) {
    loadings$.name <- rownames(loadings)
    rownames(loadings) <- NULL
  }

  structure(scores, basis = loadings)
}

#' @method fortify prcomp
#' @rdname fortify-pca
#' @export
fortify.prcomp <- function(model, data = NULL, scale = 0, equalize = scale != 0, ...) {
  scores <- if (is.null(data)) predict(model) else predict(model, data)
  .fortify_pca(scores, model$rotation, model$sdev, scale, equalize, data)
}

#' @method fortify princomp
#' @rdname fortify-pca
#' @export
fortify.princomp <- function(model, data = NULL, scale = 0, equalize = scale != 0, ...) {
  scores <- if (is.null(data)) predict(model) else predict(model, data)
  loadings <- model$loadings
  class(loadings) <- "matrix"
  .fortify_pca(scores, loadings, model$sdev, scale, equalize, data)
}

#' @method fortify PCA
#' @rdname fortify-pca
#' @export
fortify.PCA <- function(model, data = NULL, scale = 0, equalize = scale != 0, ...) {
  scores <- data.frame(pcobj$ind$coord)
  loadings <- data.frame(pcobj$var$coord)
  sdev <- sqrt(pcobj$eig[1:ncol(pcobj$var$coord), 1])
  .fortify_pca(scores, loadings, sdev, scale, equalize, data)
}
