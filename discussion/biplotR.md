# biplots in R

There are quite a few implementations of biplots in R, but IMHO, **none** are sufficiently general to become a
current standard, with wide applicability, and easily used within modern R, encompasing the latest developments
in the `tidyverse` and the `ggplot2` framework. 

Current implementation range from base R `biplot.princomp()`
to `factoextra::fviz_pca()` extending the `FactoMineR` package, and
to the wealth of tools provided by the `adegraphics` package, within
Stephane Drey's DUDI framework.

Another take on biplots was given in the `UBbipl` package accompanying Gower et al. (2011)
_Understanding Biplots_, but never released as a CRAN package. It is poorly documented,
and whenever I try to use it, I need the book handy.

My problem, as a user and explainer is that with none of these can I easily create and
describe simple examples of biplot methods with all the graphical attributes I want to 
control for a given example.

* easily allow the different scalings, to show row/column variables in principal or
  standard coordinates.
* When there are groups, provide nice data ellipses or other bivariate summaries (convex hulls, ...)
  for each group, with fine control over graphical attributes (color, fill, ...), 
  possibly direct labels for groups rather than a legend, etc.
* show row/observation points as points or observation lables. Be able to control the
  point/character size easily. Be able to filter out some point labels to highlight
  some more intersting ones.
* For variable vectors, control all graphical attributes: color, linewidth, font/sizes for
  variable names, etc.

Is it time for a new, more modern implementation that is easy to use and to understand?
Any such implementation can't do everything, but if well-designed, it could be something
many people could work with, contribute to and extend. 

For my own purposes, I've taken over the (dormant) `ggbiplot` package, forked from the original
by Vince Yu, forked in my own repo, https://github.com/friendly/ggbiplot. I'm basically
working on ideas and examples I'd like to use in my new book.

It is still incomplete, and doesn't best use tidyverse (methods to unify PCA-like objects)
and ggplot2 features (geoms for variable vectors, etc.) but it allows me to produce biplots with graphic features
I want in various examples. (There is an experimental branch in the repo, but it is still buggy.)

Does anyone want to join me in making biplots more accessible and easy to use in modern R?





