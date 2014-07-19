# 
#  ggbiplot.r
#  
#  Copyright 2011 Vincent Q. Vu.
# 
#  This program is free software; you can redistribute it and/or
#  modify it under the terms of the GNU General Public License
#  as published by the Free Software Foundation; either version 2
#  of the License, or (at your option) any later version.
#  
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#  
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
# 

#' Biplot for Principal Components using ggplot2
#'
#' @param pcobj           an object returned by prcomp() or princomp()
#' @param choices         which PCs to plot
#' @param scale           covariance biplot (scale = 1), form biplot (scale = 0). When scale = 1, the inner product between the variables approximates the covariance and the distance between the points approximates the Mahalanobis distance.
#' @param pc.biplot       for compatibility with biplot.princomp()
#' @param groups          optional factor variable indicating the groups that the observations belong to. If provided the points will be colored according to groups
#' @param ellipse         draw a normal data ellipse for each group?
#' @param ellipse.prob    size of the ellipse in Normal probability
#' @param labels          optional vector of labels for the observations
#' @param labels.size     size of the text used for the labels
#' @param alpha           alpha transparency value for the points (0 = TRUEransparent, 1 = opaque)
#' @param circle          draw a correlation circle? (only applies when prcomp was called with scale = TRUE and when var.scale = 1)
#' @param var.axes        draw arrows for the variables?
#' @param varname.size    size of the text for variable names
#' @param varname.abbrev  whether or not to abbreviate the variable names
#'
#' @return                a ggplot2 plot
#' @export
#' @examples
#'   data(wine)
#'   wine.pca <- prcomp(wine, scale. = TRUE)
#'   print(ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, groups = wine.class, ellipse = TRUE, circle = TRUE))
#'
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                      groups = NULL, ellipse = FALSE, ellipse.prob = 0.68, 
                      labels = NULL, labels.size = 3, alpha = 1, 
                      var.axes = TRUE, 
                      circle = FALSE, circle.prob = 0.69, 
                      varname.size = 3, varname.abbrev = FALSE, ...)
{
  stopifnot(length(choices) == 2)

  df <- fortify(pcobj, scale = scale)

  if(is.null(attr(df, "basis"))) {
    stop("unsupported pcobj")
  }

  u <- if(pc.biplot) sqrt(nrow(df)) * df[, choices] else df[, choices]
  v <- attr(df, "basis")[, choices]

  names(u) <- c('x', 'y')
  names(v) <- c('x', 'y')

  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(u^2))^(1/4)

  # Scale directions
  v_scale <- r / sqrt(max(rowSums(v^2)))

  uname <- names(df)[choices]

  u$label <- if(!is.null(labels)) labels else NA
  u$group <- if(!is.null(groups)) groups else NA

  v$name <- if(varname.abbrev) {
    abbreviate(attr(df, "basis")$name) 
  } else {
    attr(df, "basis")$name
  }

  # Base plot
  g <- ggplot(u, aes(x, y)) +
    coord_equal() + xlab(uname[1]) + ylab(uname[2])

  # Draw biplot axes
  if(var.axes) {
    g <- g + geom_axis(data = v, aes(label = name), 
                       textsize = varname.size, 
                       circle = circle, scale = v_scale)
  }

  # Draw points or labels
  g <- g + if(!is.null(labels)) {
    if(!is.null(groups))
      geom_text(aes(label = label, color = group), size = labels.size)
    else
      geom_text(aes(label = label), size = labels.size)
  } else {
    if(!is.null(groups))
      geom_point(aes(color = group), alpha = alpha)
    else
      geom_point(alpha = alpha)
  }

  # Overlay a concentration ellipse if there are groups
  if(!is.null(groups) && ellipse) {
    g <- g + stat_ellipse(aes(group = group, color = group), 
                          type = "norm", level = ellipse.prob)
  }

  return(g)
}
