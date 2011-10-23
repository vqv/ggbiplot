ggbiplot
========

An implementation of the biplot using ggplot2.  The package provides two functions: `ggscreeplot()` and `ggbiplot()`.
`ggbiplot` aims to be a drop-in replacement for the built-in R function `biplot.princomp()` with extended functionality 
for labeling groups, drawing a correlation circle, and adding Normal probability ellipsoids.


Installation
------------

    library(devtools)
    install_github("ggbiplot", "vqv")

Example Usage
-------------

    library(ggbiplot)
    data(wine)
    wine.pca <- prcomp(wine, scale. = TRUE)
    g <- ggbiplot(wine.pca, obs.scale = 1, var.scale = 1, 
                  groups = wine.class, ellipse = TRUE, circle = TRUE)
    g <- g + scale_color_discrete(name = '')
    g <- g + opts(legend.direction = 'horizontal', 
                  legend.position = 'top')
    print(g)