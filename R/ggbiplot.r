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
#' @param obs.scale       scale factor to apply to observations
#' @param var.scale       scale factor to apply to variables
#' @param var.factor      factor to be applied to variable vectors after scaling. This allows the variable vectors to be reflected
#'                        (\code{var.factor = -1}) or expanded in length (\code{var.factor > 1}) for greater visibility.
#' @param pc.biplot       for compatibility with biplot.princomp()
#' @param groups          optional factor variable indicating the groups that the observations belong to. 
#'                        If provided the points will be colored according to groups.
#' @param point.size      Size of observation points.
#' @param ellipse         draw a normal data ellipse for each group?
#' @param ellipse.prob    coverage size of the data ellipse in Normal probability
#' @param ellipse.linewidth    thickness of the line outlining the ellipses
#' @param labels          optional vector of labels for the observations
#' @param labels.size     size of the text used for the labels
#' @param alpha           alpha transparency value for the points (0 = transparent, 1 = opaque)
#' @param circle          draw a correlation circle? (only applies when prcomp was called with scale = TRUE and when var.scale = 1)
#' @param circle.prob     size of circle
#' @param var.axes        draw arrows for the variables?
#' @param varname.size    size of the text for variable names
#' @param varname.color   color for the variable vectors and names
#' @param varname.adjust  adjustment factor the placement of the variable names, >= 1 means farther from the arrow
#' @param varname.abbrev  whether or not to abbreviate the variable names
#' @param ...             other arguments passed down
#'
#' @import     ggplot2
#' @importFrom stats predict qchisq var
#' @importFrom scales muted
## @importFrom plyr ddply
#' @importFrom dplyr filter n summarize select group_by
#' @importFrom tidyr unnest
#' @importFrom purrr map
#' @return                a ggplot2 plot object
#' @export
#' @examples
#'   data(wine)
#'   library(ggplot2)
#'   wine.pca <- prcomp(wine, scale. = TRUE)
#'   ggbiplot(wine.pca, 
#'            obs.scale = 1, var.scale = 1, 
#'            varname.size = 4,
#'            groups = wine.class, 
#'            ellipse = TRUE, circle = TRUE)
#'
ggbiplot <- function(pcobj, choices = 1:2, scale = 1, pc.biplot = TRUE, 
                     obs.scale = 1 - scale, 
                     var.scale = scale, 
                     var.factor = 1,    # MF
                     groups = NULL, 
                     point.size = 1.5,
                     ellipse = FALSE, 
                     ellipse.prob = 0.68, 
                     ellipse.linewidth = 1.3,
                     labels = NULL, labels.size = 3, 
                     alpha = 1, 
                     var.axes = TRUE, 
                     circle = FALSE, circle.prob = 0.68, 
                     varname.size = 3, 
                     varname.adjust = 1.5, 
                     varname.color = 'darkred',
                     varname.abbrev = FALSE, ...)
{
  # library(ggplot2)
  # library(plyr)
  # library(scales)
  # library(grid)

  stopifnot(length(choices) == 2)

  # Recover the SVD
 if(inherits(pcobj, 'prcomp')){
    nobs.factor <- sqrt(nrow(pcobj$x) - 1)
    d <- pcobj$sdev
    u <- sweep(pcobj$x, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$rotation
  } else if(inherits(pcobj, 'princomp')) {
    nobs.factor <- sqrt(pcobj$n.obs)
    d <- pcobj$sdev
    u <- sweep(pcobj$scores, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- pcobj$loadings
  } else if(inherits(pcobj, 'PCA')) {
    nobs.factor <- sqrt(nrow(pcobj$call$X))
    d <- unlist(sqrt(pcobj$eig)[1])
    u <- sweep(pcobj$ind$coord, 2, 1 / (d * nobs.factor), FUN = '*')
    v <- sweep(pcobj$var$coord,2,sqrt(pcobj$eig[1:ncol(pcobj$var$coord),1]),FUN="/")
  } else if(inherits(pcobj, "lda")) {
      nobs.factor <- sqrt(pcobj$N)
      d <- pcobj$svd
      u <- predict(pcobj)$x/nobs.factor
      v <- pcobj$scaling
      d.total <- sum(d^2)
  } else {
    stop('Expected a object of class prcomp, princomp, PCA, or lda')
  }

  # shutup 'no visible binding...'
#  utils::globalVariables(c("xvar", "yvar", "varname", "angle", "hjust"))
  angle <- circle_chol <- ed <- hjust <- mu <- sigma <- varname <- xvar <- yvar <-NULL

  # Scores
  choices <- pmin(choices, ncol(u))
  df.u <- as.data.frame(sweep(u[,choices], 2, d[choices]^obs.scale, FUN='*'))

  # Directions
  v <- sweep(v, 2, d^var.scale, FUN='*')
  df.v <- as.data.frame(v[, choices])
  df.v <- var.factor * df.v

  names(df.u) <- c('xvar', 'yvar')
  names(df.v) <- names(df.u)

  if(pc.biplot) {
    df.u <- df.u * nobs.factor
  }

  # Scale the radius of the correlation circle so that it corresponds to 
  # a data ellipse for the standardized PC scores
  r <- sqrt(qchisq(circle.prob, df = 2)) * prod(colMeans(df.u^2))^(1/4)

  # Scale directions
  v.scale <- rowSums(v^2)
  df.v <- r * df.v / sqrt(max(v.scale))

  # Change the labels for the axes
  if(obs.scale == 0) {
    u.axis.labs <- paste('standardized PC', choices, sep='')
  } else {
    u.axis.labs <- paste('PC', choices, sep='')
  }

  # Append the proportion of explained variance to the axis labels
  u.axis.labs <- paste(u.axis.labs, 
                       sprintf('(%0.1f%%)', 
                               100 * pcobj$sdev[choices]^2/sum(pcobj$sdev^2)))

  # Score Labels
  if(!is.null(labels)) {
    df.u$labels <- labels
  }

  # Grouping variable
  if(!is.null(groups)) {
    df.u$groups <- groups
  }

  # Variable Names
  if(varname.abbrev) {
    df.v$varname <- abbreviate(rownames(v))
  } else {
    df.v$varname <- rownames(v)
  }

  # Variables for text label placement
  df.v$angle <- with(df.v, (180/pi) * atan(yvar / xvar))
  df.v$hjust = with(df.v, (1 - varname.adjust * sign(xvar)) / 2)

  # Base plot
  g <- ggplot(data = df.u, aes(x = xvar, y = yvar)) + 
          xlab(u.axis.labs[1]) + 
          ylab(u.axis.labs[2]) + 
          coord_equal()

  # Draw either labels or points
  if(!is.null(df.u$labels)) {
    if(!is.null(df.u$groups)) {
      g <- g + geom_text(aes(label = labels, color = groups), 
                         size = labels.size)
    } else {
      g <- g + geom_text(aes(label = labels), size = labels.size)      
    }
  } else {
    if(!is.null(df.u$groups)) {
      g <- g + geom_point(aes(color = groups), alpha = alpha, size = point.size)
    } else {
      g <- g + geom_point(alpha = alpha)      
    }
  }
  
  if(var.axes) {
    # Draw circle
    if(circle) 
    {
      theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
      circle <- data.frame(xvar = r * cos(theta), yvar = r * sin(theta))
      g <- g + geom_path(data = circle, 
                         color = scales::muted('white'), 
                         linewidth = 1/2, alpha = 1/3)
    }

    # Draw directions
    arrow_style <- arrow(length = unit(1/2, 'picas'), type="closed", angle=15) 
    g <- g +
      geom_segment(data = df.v,
                   aes(x = 0, y = 0, xend = xvar, yend = yvar),
                   arrow = arrow_style, 
                   color = varname.color,
                   linewidth = 1.4)    # MR: was 1.2
  }

  # Overlay a concentration ellipse if there are groups
  if(!is.null(df.u$groups) && ellipse) {
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- cbind(cos(theta), sin(theta))

    # ell <- ddply(df.u, 'groups', function(x) {
    #   if(nrow(x) <= 2) {
    #     return(NULL)
    #   }
    #   sigma <- var(cbind(x$xvar, x$yvar))
    #   mu <- c(mean(x$xvar), mean(x$yvar))
    #   ed <- sqrt(qchisq(ellipse.prob, df = 2))
    #   data.frame(sweep(circle %*% chol(sigma) * ed, 2, mu, FUN = '+'), 
    #              groups = x$groups[1])
    # })
    # names(ell)[1:2] <- c('xvar', 'yvar')

    ell <- 
      df.u |>
      group_by(groups) |>
      filter(n() > 2) |>
      summarize(
        sigma = list(var(cbind(xvar, yvar))),
        mu = list(c(mean(xvar), mean(yvar))),
        ed = sqrt(qchisq(ellipse.prob, df = 2)),
        circle_chol = list(circle %*% chol(sigma[[1]]) * ed),
        ell = list(sweep(circle_chol[[1]], 2, mu[[1]], FUN = "+")),
        xvar = map(ell, ~.x[,1]),
        yvar = map(ell, ~.x[,2]),
        .groups = "drop"
      ) |> 
      dplyr::select(xvar, yvar, groups) |> 
      tidyr::unnest(c(xvar, yvar))

    # g <- g + geom_path(data = ell, 
    #                    aes(color = groups, 
    #                        group = groups),
    #                    linewidth = ellipse.linewidth)
    # g <- g + geom_polygon(data = ell, 
    #                       aes(color = groups, 
    #                           fill = groups
    #                         #  group = groups
    #                           ),
    #                       alpha = 0.4,    # MF: why doesn't this have any effect?
    #                       linewidth = ellipse.linewidth)

    # Overlay a concentration ellipse if there are groups
      g <- g + stat_ellipse(geom="polygon",
                            aes(group = groups, 
                                color = groups,
                                fill = groups),
                            alpha = 0.2,
                            linewidth = ellipse.linewidth,
                            type = "norm", level = ellipse.prob)

  }

  # Label the variable axes
  if(var.axes) {
    g <- g + 
    geom_text(data = df.v, 
              aes(label = varname, x = xvar, y = yvar, 
                  angle = angle, hjust = hjust), 
              color = varname.color, size = varname.size)
  }
  # Change the name of the legend for groups
  # if(!is.null(groups)) {
  #   g <- g + scale_color_brewer(name = deparse(substitute(groups)), 
  #                               palette = 'Dark2')
  # }

  # TODO: Add a second set of axes

  return(g)
}
