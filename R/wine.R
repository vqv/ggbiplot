#' @title Wine dataset
#' @name wine
#' @aliases wine.class
#' @description 
#'    Results of a chemical analysis of wines grown in the same region in Italy,
#'    derived from three different cultivars. The analysis determined the 
#'    quantities of 13 chemical constituents found in each of the three types of wines. 
#'    
#'    The grape varieties (cultivars), 'barolo', 'barbera', and 'grignolino', are indicated in \code{wine.class}.
#'
#' 
#' @docType data
#' @usage data(wine)
#' @format 
#' A \code{wine} data frame consisting of 178 observations (rows) and
#' 13 columns and vector \code{wine.class} of factors indicating the cultivars.
#' The variables are:
#'   \describe{
#'      \item{\code{Alcohol}}{a numeric vector}
#'      \item{\code{MalicAcid}}{Malic acid, a numeric vector}
#'      \item{\code{Ash}}{Ash, a numeric vector}
#'      \item{\code{AlcAsh}}{Alcalinity of ash, a numeric vector}
#'      \item{\code{Mg}}{Magnesium, a numeric vector}
#'      \item{\code{Phenols}}{total phenols, a numeric vector}
#'      \item{\code{Flav}}{Flavanoids, a numeric vector}
#'      \item{\code{NonFlavPhenols}}{Nonflavanoid phenols, a numeric vector}
#'      \item{\code{Proa}}{Proanthocyanins, a numeric vector}
#'      \item{\code{Color}}{Color intensity, a numeric vector}
#'      \item{\code{Hue}}{a numeric vector}
#'      \item{\code{OD}}{D280/OD315 of diluted wines, a numeric vector}
#'      \item{\code{Proline}}{a numeric vector}
#'      }

#' @source UCI Machine Learning Repository (\url{http://archive.ics.uci.edu/ml/datasets/Wine})
#' @keywords dataset
#'
#' @examples 
#' data(wine)
#' table(wine.class)
#' 
#' wine.pca <- prcomp(wine, scale. = TRUE)
#' ggscreeplot(wine.pca)                                               
#' ggbiplot(wine.pca, 
#'          obs.scale = 1, var.scale = 1, 
#'          groups = wine.class, ellipse = TRUE, circle = TRUE)

NULL
