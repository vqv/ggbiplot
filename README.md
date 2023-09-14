
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ggbiplot)](https://CRAN.R-project.org/package=ggbiplot)
[![Last
Commit](https://img.shields.io/github/last-commit/friendly/ggbiplot)](https://github.com/friendly/ggbiplot)
<!-- badges: end -->

This repo for the [ggbiplot
package](https://CRAN.R-project.org/package=ggbiplot) was forked from
<https://github.com/vqv/ggbiplot> by Vince Yu, which has been dormant
since 2015.

The goal is to complete that development and publish a new version on
CRAN. There is also an [experimental
branch](https://github.com/friendly/ggbiplot/tree/experimental) which
attempts to simplify the code, but this has some unresolved problems.

# ggbiplot

An implementation of the biplot using ggplot2. The package provides two
functions: `ggscreeplot()` and `ggbiplot()`. `ggbiplot` aims to be a
drop-in replacement for the built-in R function `biplot.princomp()` with
extended functionality for labeling groups, drawing a correlation
circle, and adding Normal probability ellipsoids.

## Installation

Install the current master branch with:

``` r
remotes::install_github("friendly/ggbiplot")
```

## Example Usage

The `wine` data contains results of a chemical analysis of wines grown
in the same region in Italy, derived from three different cultivars. The
analysis determined the quantities of 13 chemical constituents found in
each of the three types of wines. The grape varieties (cultivars),
‘barolo’, ‘barbera’, and ‘grignolino’, are given in `wine.class`.

What can we understand about the differences among these wines from a
biplot?

``` r
library(ggbiplot)
library(ggplot2)
data(wine)
wine.pca <- prcomp(wine, scale. = TRUE)
ggscreeplot(wine.pca)
```

![](man/figures/README-wine-screeplot-1.png)<!-- -->

Plot the first two PCA dimensions:

``` r
ggbiplot(wine.pca, 
  obs.scale = 1, var.scale = 1,
  groups = wine.class, 
  ellipse = TRUE, 
  circle = TRUE) +
  scale_color_discrete(name = '') +
  theme(legend.direction = 'horizontal', legend.position = 'top')
```

![](man/figures/README-wine-biplot-1.png)<!-- -->
