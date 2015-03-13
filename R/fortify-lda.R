#' Fortify method for principal components analysis objects produced 
#' by \code{lda} in \pkg{MASS}
#' 
#' @name fortify-lda
#'
#' @param model   an object of class \code{\link[MASS]{lda}}
#' @param data    discriminant scores and class predictions are computed for data 
#' @param scale   scale the scores and loadings as done by \code{\link[stats]{biplot.prcomp}}
#' @param equalize should the basis vectors be rescaled to be comparable with the score vectors?
#' @param ...     not used
#'
#' @return A data frame consisting of the columns of \code{data} together with 
#'         discriminant scores and class predictions.  Additionally, the attribute 
#'         \code{basis} is set to a data frame containing the loadings and a 
#'         column of variable names if present in \code{model}
#'
#' @examples
#' library(MASS)
#' m <- lda(Species ~ ., data = iris)
#' df <- fortify(m, iris)
#' ggplot(df, aes(x = LD1, y = LD2)) +
#'   geom_point(aes(group = Species, color = Species, shape = Species)) + 
#'   stat_ellipse(aes(group = Species, color = Species)) +
#'   geom_axis(data = attr(df, "basis"), aes(label = abbreviate(.name))) + 
#'   geom_point(data = fortify(irislda, data.frame(m$means)), shape = 3) + 
#'   ylim(-4, 4) + coord_equal()
#' @export
fortify.lda <- function(model, data = NULL, scale = 0, equalize = scale != 0, ...) {

  # Predict
  fit <- if (is.null(data)) {
    predict(model, ...)
  } else {
    predict(model, data, ...)
  }

  # Rescale
  scores <- sweep(fit$x, 2, model$svd^(-scale), FUN = "*")
  loadings <- sweep(model$scaling, 2, model$svd^scale, FUN = "*")

  if(equalize) {
    r <- sqrt( median(rowSums(scores^2)) / max(colSums(loadings^2)) )
    loadings <- loadings * r
  }

  scores <- data.frame(scores, .class = fit$class)
  loadings <- data.frame(loadings)

  if (!is.null(data)) scores <- cbind(data, scores)
  if (!is.null(rownames(loadings))) {
    loadings$.name <- rownames(loadings)
    rownames(loadings) <- NULL
  }

  structure(scores, basis = loadings)
}
