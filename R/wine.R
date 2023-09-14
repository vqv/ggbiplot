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
#' @source UCI Machine Learning Repository (\url{http://archive.ics.uci.edu/ml/datasets/Wine})
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
